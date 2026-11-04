# Regression tests for the issues found in the second code review.

test_that("lavaan syntax that could run R code is rejected and never executed", {
  d <- make_mediation_data()
  marker <- file.path(tempdir(), "lavaan_code_exec_marker")
  unlink(marker)
  bad <- sprintf('m1 ~ start(file.create("%s"))*x1\ny1 ~ m1', marker)
  r <- fit_lavaan_model(bad, d)
  expect_equal(r$status, "failed")
  expect_match(r$errors[1], "not allowed")
  expect_false(file.exists(marker))
  expect_false(check_lavaan_syntax("y1 ~ x1\nz := system(1) + 1")$ok)
  expect_false(check_lavaan_syntax("y1 ~ `x1`")$ok)
  expect_false(check_lavaan_syntax("y1 ~ x1\nz := base::exp(1)")$ok)
  # legitimate lavaan syntax still passes
  ok <- "m1 ~ start(0.5)*x1 + a*x1\ny1 ~ b*m1 + c(cp)*x1\nind := a*b\nr := exp(a) / sqrt(abs(b))\na > -1"
  expect_true(check_lavaan_syntax(ok, names(d))$ok)
  expect_true(check_lavaan_syntax("F =~ NA*m1 + y1 + y2\nF ~~ 1*F", names(d))$ok)
})

test_that("BCa falls back to bias-corrected intervals when there are too few draws", {
  d <- make_mediation_data(n = 250)
  r <- fit_lavaan_model("m1 ~ a*x1\ny1 ~ b*m1\nind := a*b", d,
                        list(bootstrap = 100, boot_ci = "bca", seed = 3))
  expect_true(r$status %in% c("ok", "warning"))
  expect_gt(nrow(r$params), 0)
  expect_true(any(grepl("bca.simple", r$notes)))
  expect_equal(attr(r$params, "ci_method"), "bootstrap-bca.simple")
})

test_that("small samples relative to model size are noted; arguments are recorded", {
  d <- make_mediation_data(n = 30)
  r <- fit_lavaan_model("m1 ~ x1 + x2\ny1 ~ m1 + x1 + x2\ny2 ~ m1 + y1", d)
  expect_true(any(grepl("cases per free parameter", r$notes)))
  expect_equal(r$lavaan_args$estimator, "ML")
  expect_equal(r$lavaan_args$missing, "fiml")
})

test_that("only clearly inadmissible lavaan warnings change the status", {
  expect_true(is_inadmissible_warning("the covariance matrix of the residuals of the observed variables (theta) is not positive definite"))
  expect_true(is_inadmissible_warning("could not compute standard errors! The information matrix could not be inverted."))
  expect_false(is_inadmissible_warning("The variance-covariance matrix of the estimated parameters (vcov) does not appear to be positive definite! The smallest eigenvalue (= 1e-18) is close to zero. This may be a symptom that the model is not identified."))
})

test_that("large UTF-8 files are not mistaken for GBK when the sample cuts a character", {
  f <- tempfile(fileext = ".csv")
  line <- "满意度,压力\n"
  con <- file(f, "wb"); for (i in 1:3) writeBin(charToRaw(enc2utf8(line)), con); close(con)
  # 7 bytes ends in the middle of the third character of the first line
  expect_equal(detect_text_encoding(f, max_bytes = 7), "GB18030")   # no complete line to inspect
  expect_equal(detect_text_encoding(f, max_bytes = nchar(enc2utf8(line), "bytes") + 4), "UTF-8")
  u16 <- tempfile(fileext = ".csv")
  writeBin(c(as.raw(c(0xff, 0xfe)), iconv("a,b\n1,2\n", "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]), u16)
  expect_equal(detect_text_encoding(u16), "UTF-16LE")
  expect_equal(read_data_file(u16)$data$b, 2)
})

test_that("rows with fewer fields than the header are padded with NA", {
  f <- tempfile(fileext = ".csv")
  writeLines(c("a,b,c", "1,2,3", "4,5"), f)
  d <- read_data_file(f)$data
  expect_equal(dim(d), c(2L, 3L))
  expect_true(is.na(d$c[2]))
})

test_that("variables with few categories are flagged as ordinal-like", {
  d <- describe_data(data.frame(likert = c(1, 2, 3, 2, 1), cont = c(0.1, 2.5, 3.3, 1.2, 0.7), sex = c(0, 1, 0, 1, 1)))
  expect_equal(d$flag_ordinal, c(TRUE, FALSE, TRUE))
})

test_that("numbered constructs (M1, M2) are not linked automatically", {
  expect_equal(unname(suggest_links(c("X", "M1", "M2", "Y"))), rep("", 4))
  expect_equal(unname(suggest_links(c("X_1", "X_2"))), c("X", "X"))
})

test_that("manifest uses {{placeholder}} columns and a template id", {
  models <- list(
    list(id = "M001", name = "a", engine = "lavaan", syntax = "y ~ x", template = "{{Y}} ~ {{X}}", assignment = c(Y = "y", X = "x")),
    list(id = "M002", name = "b", engine = "lavaan", syntax = "y ~ w", template = "{{Y}} ~ {{X}}", assignment = c(Y = "y", X = "w")),
    list(id = "M003", name = "c", engine = "lavaan", syntax = "y ~ q", template = "{{se}} ~ q", assignment = c(se = "y")),
    list(id = "M004", name = "d", engine = "lavaan", syntax = "y ~ z", template = "# edited comment\n{{Y}}  ~ {{X}}", assignment = c(Y = "y", X = "z")))
  m <- build_manifest(models)
  expect_equal(m$template_id, c("T1", "T1", "T2", "T1"))
  expect_equal(manifest_ph_cols(m), c("{{Y}}", "{{X}}", "{{se}}"))
  expect_true(is.na(m[["{{X}}"]][3]))
  expect_equal(int_input(NA, 5L), 5L)
  expect_equal(num_input(NULL, 0.95), 0.95)
})

fake_fit <- function(id, est, p, label = "ind") {
  r <- new_model_result(id, "lavaan")
  r$status <- "ok"
  r$params <- data.frame(group = "", lhs = c(label, "dep"), op = c(":=", "~"), rhs = c("a*b", "sat"),
                         label = c(label, ""), est = c(est, 0.3), se = c(0.05, 0.1), z = c(est / 0.05, 3),
                         p = c(p, 0.003), ci_lower = c(est - 0.1, 0.1), ci_upper = c(est + 0.1, 0.5),
                         std_est = c(est, 0.2), std_se = NA, std_p = NA, std_ci_lower = NA, std_ci_upper = NA,
                         stringsAsFactors = FALSE)
  r
}

test_that("a placeholder called {{se}} or {{p}} does not overwrite result columns", {
  man <- data.frame(id = c("M001", "M002"), name = "x", syntax = "", template_id = "T1",
                    "{{se}}" = c("selfesteem1", "selfesteem2"), "{{p}}" = c("pa", "pb"),
                    check.names = FALSE, stringsAsFactors = FALSE)
  res <- list(M001 = fake_fit("M001", 0.2, 0.001), M002 = fake_fit("M002", 0.1, 0.04))
  f <- build_focal_table(res, man, types = "defined")
  expect_equal(f$se, c(0.05, 0.05))
  expect_equal(f$p, c(0.001, 0.04))
  expect_equal(f[["{{se}}"]], c("selfesteem1", "selfesteem2"))
})

test_that("effect keys are template-specific and labels match case-insensitively", {
  man <- data.frame(id = c("M001", "M002", "M003"), name = "x", syntax = "", template_id = c("T1", "T1", "T2"),
                    "{{X}}" = c("sat", "str", NA), "{{Y}}" = "dep", check.names = FALSE, stringsAsFactors = FALSE)
  res <- list(M001 = fake_fit("M001", 0.2, 0.001), M002 = fake_fit("M002", 0.1, 0.04), M003 = fake_fit("M003", 0.3, 0.01))
  f <- build_focal_table(res, man, types = c("defined", "regression"))
  expect_setequal(unique(f$effect_key[grepl("ind", f$effect_key)]), c("[T1] ind", "[T2] ind"))
  expect_true("[T1] {{Y}} ← {{X}}" %in% f$effect_key)
  expect_equal(nrow(build_focal_table(res, man, types = "defined", labels = "IND")), 3)
})

test_that("the specification curve draws a panel of variable choices", {
  man <- data.frame(id = sprintf("M%03d", 1:4), name = "x", syntax = "", template_id = "T1",
                    "{{X}}" = c("a", "b", "a", "b"), "{{M}}" = c("m1", "m1", "m2", "m2"),
                    check.names = FALSE, stringsAsFactors = FALSE)
  res <- stats::setNames(lapply(seq_len(4), function(i) fake_fit(man$id[i], i / 10, 0.01)), man$id)
  f <- build_focal_table(res, man, types = "defined")
  rows <- spec_panel_rows(f[order(f$est), ])
  expect_equal(length(rows), 4)
  expect_equal(rows[[1]]$label, "{{X}} = a")
  png(tempfile(fileext = ".png")); on.exit(dev.off())
  expect_silent(plot_spec_curve(f, "ind"))
})

test_that("reproduce.R uses each model's own lavaan arguments and reproduces the estimates", {
  d <- make_mediation_data()
  d$u <- cut(d$y2, c(-Inf, -0.5, 0.5, Inf), labels = FALSE)
  opts <- list(ordered = "u", seed = 11)
  syn <- c(M001 = "m1 ~ a*x1\ny1 ~ b*m1\nind := a*b", M002 = "u ~ x1")
  res <- lapply(names(syn), function(id) fit_lavaan_model(syn[[id]], d, opts, id = id))
  names(res) <- names(syn)
  expect_equal(res$M001$lavaan_args$estimator, "ML")        # no ordered variable in M001
  expect_equal(res$M002$lavaan_args$estimator, "WLSMV")
  man <- data.frame(id = names(syn), name = names(syn), syntax = unname(syn), stringsAsFactors = FALSE)
  z <- tempfile(fileext = ".zip")
  write_results_zip(z, man, "lavaan", d, NULL, res, NULL, opts)
  dir <- tempfile("unzipped"); utils::unzip(z, exdir = dir)
  script <- readLines(file.path(dir, "reproduce.R"))
  expect_true(any(grepl('M001 = list(estimator = "ML"', script, fixed = TRUE)))
  out <- callr::r(function(dir) {
    setwd(dir)
    source("reproduce.R", local = TRUE)
    lavaan::parameterEstimates(fits$M001)$est
  }, args = list(dir = dir))
  ref <- res$M001$params
  expect_equal(out[1:2], ref$est[1:2], tolerance = 1e-6)
})
