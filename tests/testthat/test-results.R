fake_result <- function(id, est, se, p, label = "ind", status = "ok") {
  r <- new_model_result(id, "lavaan")
  r$status <- status
  r$params <- data.frame(group = "", lhs = c(label, "y"), op = c(":=", "~"), rhs = c("a*b", "x"),
                         label = c(label, ""), est = c(est, 0.3), se = c(se, 0.1), z = c(est / se, 3),
                         p = c(p, 0.003), ci_lower = c(est - 2 * se, 0.1), ci_upper = c(est + 2 * se, 0.5),
                         std_est = c(est, 0.2), std_se = NA, std_p = NA, std_ci_lower = NA, std_ci_upper = NA,
                         stringsAsFactors = FALSE)
  r$fit[] <- c(100, 7, 0, 0, NA, 1, 1, 0, 0, 0, 0, 1000, 1010)
  r
}

test_that("focal effects are corrected over the whole family", {
  man <- data.frame(id = c("M001", "M002", "M003"), name = c("a", "b", "c"), syntax = "", X = c("x1", "x2", "x3"))
  res <- list(M001 = fake_result("M001", 0.2, 0.05, 0.0001),
              M002 = fake_result("M002", 0.1, 0.05, 0.03),
              M003 = fake_result("M003", 0.0, 0.05, 0.9, status = "inadmissible"))
  f <- build_focal_table(res, man, types = "defined", method = "holm")
  expect_equal(nrow(f), 2)                 # inadmissible excluded by default
  expect_equal(f$p_adj, stats::p.adjust(c(0.0001, 0.03), "holm"))
  expect_equal(f$family_size, c(2, 2))
  f2 <- build_focal_table(res, man, types = "defined", method = "bonferroni", include_inadmissible = TRUE)
  expect_equal(nrow(f2), 3)
  expect_equal(f2$p_adj[2], 0.09)
  expect_false(f2$sig_adj[2])
  s <- summarise_focal(f2)
  expect_equal(s$n_models, 3)
  expect_null(build_focal_table(res, man, types = "loading"))
})

test_that("effect keys are template-level (variables replaced by placeholders)", {
  p <- data.frame(group = "", lhs = c("dep", "dep"), op = c("~", "ind"), rhs = c("sat", "sat"),
                  label = c("", "sat -> sup -> dep"), stringsAsFactors = FALSE)
  e <- effect_descriptions(p, c(X = "sat", M = "sup", Y = "dep"))
  expect_equal(e$key[2], "{{X}} -> {{M}} -> {{Y}}")
  expect_match(e$key[1], "\\{\\{Y\\}\\}.*\\{\\{X\\}\\}")
  expect_match(e$desc[1], "dep")
})

test_that("fit judgement flags saturated models", {
  j <- fit_judgement(c(1, .96, .92, .80), c(1, .96, .91, .8), c(0, .04, .07, .12), c(0, .03, .07, .12), c(0, 5, 5, 5))
  expect_equal(j, c("saturated", "good", "acceptable", "poor"))
})

test_that("fit table and exports", {
  man <- data.frame(id = c("M001", "M002"), name = c("a", "b"), syntax = c("y ~ x", "y ~ z"), X = c("x", "z"),
                    stringsAsFactors = FALSE)
  res <- list(M001 = fake_result("M001", .2, .05, .001), M002 = fake_result("M002", .1, .05, .04))
  ft <- build_fit_table(res, man)
  expect_equal(ft$id, c("M001", "M002"))
  expect_true(all(ft$saturated))
  x <- tempfile(fileext = ".xlsx")
  write_results_xlsx(x, list(Fit = ft, Focal = build_focal_table(res, man)))
  expect_equal(openxlsx::getSheetNames(x), c("Fit", "Focal"))
  z <- tempfile(fileext = ".zip")
  write_results_zip(z, man, "lavaan", data.frame(x = 1:3), x, res, NULL, lavaan_default_options())
  files <- zip::zip_list(z)$filename
  expect_true(all(c("results.xlsx", "manifest.csv", "data.csv", "reproduce.R", "models/M001.lav") %in% files))
  m <- methods_table(list(engine = "lavaan", opts = lavaan_default_options(), notes = "note"), "zh")
  expect_true(any(grepl("种子", m[[1]])))
})
