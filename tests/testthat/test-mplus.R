test_that("Mplus names respect 8 characters and case-insensitivity", {
  nm <- mplus_name_map(c("x1", "X1", "satisfaction_total", "satisfaction_t2", "2wave", "满意度"))
  expect_true(all(nchar(nm$mplus) <= 8))
  expect_equal(anyDuplicated(toupper(nm$mplus)), 0)
  expect_equal(nm$mplus[1], "x1")
  expect_true(grepl("^[A-Za-z]", nm$mplus[5]))
  expect_equal(nm$mplus[6], "V6")
  expect_equal(unmap_mplus_names(toupper(nm$mplus[3]), nm), "satisfaction_total")
})

test_that("input files: generated header, conflicting statements replaced, renaming", {
  nm <- mplus_name_map(c("satisfaction_total", "x", "y"))
  tpl <- "TITLE: old\nDATA: FILE = old.dat; LISTWISE = ON;\nVARIABLE:\n  NAMES = a b c;\n  USEVARIABLES = satisfaction_total x;\n  MISSING ARE ALL (-999);\nMODEL:\n  satisfaction_total ON x;\n"
  b <- build_mplus_input(tpl, nm, "M001")
  txt <- b$text
  expect_match(txt, "FILE = \"data.dat\";", fixed = TRUE)
  expect_match(txt, "LISTWISE = ON;", fixed = TRUE)
  expect_match(txt, "MISSING = .;", fixed = TRUE)
  expect_false(grepl("old.dat", txt, fixed = TRUE))
  expect_false(grepl("-999", txt, fixed = TRUE))
  expect_false(grepl("NAMES = a b c", txt, fixed = TRUE))
  expect_match(txt, "satisfac ON x;", fixed = TRUE)
  expect_equal(b$missing_codes, -999)
  expect_true(any(grepl("MISSING", b$notes)))
  expect_true(all(nchar(strsplit(txt, "\n")[[1]]) <= 90))
})

test_that("long lines are wrapped below 90 characters", {
  vars <- sprintf("var%05d", 1:30)
  nm <- mplus_name_map(c(vars, "y"))
  b <- build_mplus_input(paste0("VARIABLE: USEVARIABLES = y ", paste(vars, collapse = " "), ";\nMODEL: y ON ",
                                paste(vars, collapse = " "), "; ! a comment"), nm, "M001")
  expect_true(all(nchar(strsplit(b$text, "\n")[[1]]) <= 90))
  expect_match(b$text, "! a comment", fixed = TRUE)
})

test_that("missing USEVARIABLES and unknown variables are reported", {
  nm <- mplus_name_map(c("x", "y"))
  expect_true(any(grepl("USEVARIABLES", build_mplus_input("MODEL: y ON x;", nm)$notes)))
  b <- build_mplus_input("VARIABLE: USEV = x y z XW;\nDEFINE: XW = x*y;\nMODEL: y ON x;", nm)
  expect_true(any(grepl("z$", b$notes)))
  expect_false(any(grepl("XW", b$notes)))
})

test_that("data file uses '.' for missing values and rejects text columns", {
  f <- tempfile()
  write_mplus_data(data.frame(a = c(1, NA), b = c(0.00001, 2)), f)
  expect_equal(readLines(f), c("1 0.00001", ". 2"))
  expect_error(write_mplus_data(data.frame(a = "x"), f), "numeric")
})

test_that("MplusAutomation tables are converted to the unified format", {
  u <- data.frame(paramHeader = c("M.ON", "Y.ON", "Y.ON", "Intercepts", "Residual.Variances", "New.Additional.Parameters"),
                  param = c("X", "M", "X", "Y", "M", "IND"),
                  est = c(0.5, 0.4, 0.1, 0, -0.2, 0.2), se = c(0.05, 0.05, 0.05, 0.1, 0.05, 0.03),
                  est_se = c(10, 8, 2, 0, -4, 6.7), pval = c(0, 0, 0.04, 1, 0, 0), stringsAsFactors = FALSE)
  ci <- data.frame(paramHeader = u$paramHeader, param = u$param, low.5 = 0, low2.5 = u$est - 0.1,
                   low5 = 0, est = u$est, up5 = 0, up2.5 = u$est + 0.1, up.5 = 0)
  s <- transform(u, est = est / 2)
  p <- mplus_param_block(u, ci, s, NULL)
  expect_equal(p$op, c("~", "~", "~", "~1", "~~", ":="))
  expect_equal(p$lhs[1:3], c("M", "Y", "Y"))
  expect_equal(p$rhs[1:3], c("X", "M", "X"))
  expect_equal(p$ci_lower[2], 0.3)
  expect_equal(p$std_est[1], 0.25)
  expect_equal(p$label[6], "IND")

  ind <- list(
    unstandardized = list(
      overall = data.frame(pred = "X", outcome = "Y", summary = c("Total", "Total indirect"),
                           est = c("0.3", "0.2"), se = "0.05", est_se = "4", pval = "0.000", stringsAsFactors = FALSE),
      specific = data.frame(pred = "X", intervening = "M", outcome = "Y", est = "0.2", se = "0.04",
                            est_se = "5", pval = "0.000", stringsAsFactors = FALSE)),
    ci.unstandardized = list(
      specific = data.frame(pred = "X", intervening = "M", outcome = "Y", low.5 = "0", low2.5 = "0.12",
                            low5 = "0", est = "0.2", up5 = "0", up2.5 = "0.29", up.5 = "0", stringsAsFactors = FALSE))
  )
  pi <- mplus_indirect_block(ind)
  expect_equal(pi$op, c("total", "total_ind", "ind"))
  expect_equal(pi$label[3], "X -> M -> Y")
  expect_equal(pi$ci_upper[3], 0.29)
  nm <- data.frame(original = c("x_long_name", "M", "Y"), mplus = c("x_long_n", "M", "Y"), changed = c(TRUE, FALSE, FALSE))
  pi$rhs <- toupper(c("x_long_n", "x_long_n", "x_long_n"))
  expect_equal(unmap_mplus_names(pi$rhs, nm), rep("x_long_name", 3))
})

test_that("output text checks detect errors and inadmissible solutions", {
  f <- tempfile(fileext = ".out")
  writeLines(c("*** ERROR in MODEL command", "  Unknown variable: Q", ""), f)
  r <- parse_mplus_output(f, "M001")
  expect_equal(r$status, "failed")
  expect_true(any(grepl("ERROR", r$errors)))
})

test_that("reserved Mplus words are never used as variable names", {
  nm <- mplus_name_map(c("with", "ON", "age", "by"))
  expect_false(any(toupper(nm$mplus) %in% MPLUS_RESERVED))
  expect_equal(nm$mplus[3], "age")
})

test_that("lines with labels are not broken (a label applies to its whole line)", {
  vars <- sprintf("v%02d", 1:20)
  long <- paste0("  y ON ", paste(vars, collapse = " "), " (b);")
  w <- wrap_mplus_lines(paste0("MODEL:\n", long))
  expect_true(grepl(long, w$text, fixed = TRUE))
  expect_true(any(grepl("not broken", w$notes)))
  # only the comment is too long: it moves to its own line
  w2 <- wrap_mplus_lines(paste0("  y ON x (b); ! ", strrep("comment ", 12)))
  expect_true(all(nchar(strsplit(w2$text, "\n")[[1]]) <= 90))
  expect_match(w2$text, "y ON x (b);", fixed = TRUE)
})

test_that("FATAL ERROR messages are detected; text variables are left out for Mplus", {
  f <- tempfile(fileext = ".out")
  writeLines(c("*** FATAL ERROR", "  THERE IS NOT ENOUGH MEMORY SPACE"), f)
  r <- parse_mplus_output(f)
  expect_true(any(grepl("FATAL", r$errors)))
  md <- mplus_numeric_data(data.frame(id = c("a", "b"), x = 1:2, flag = c(TRUE, FALSE)))
  expect_equal(names(md$data), c("x", "flag"))
  expect_equal(md$dropped, "id")
})

test_that("R-square rows are read from Mplus output tables", {
  mo <- list(parameters = list(
    unstandardized = data.frame(paramHeader = "Y.ON", param = "X", est = 0.5, se = 0.1, est_se = 5, pval = 0),
    r2 = data.frame(param = "Y", est = 0.25, se = 0.04, est_se = 6.2, pval = 0)))
  p <- mplus_params_table(mo, NULL)
  expect_equal(p$op, c("~", "r2"))
  expect_equal(p$std_est[2], 0.25)
})

test_that("Bayesian and multilevel parameter tables are handled", {
  u <- data.frame(paramHeader = c("Y.ON", "Y.ON"), param = c("X", "X"), est = c(0.5, 0.2),
                  posterior_sd = c(0.1, 0.05), pval = c(0, 0.01), `lower_2.5ci` = c(0.3, 0.1),
                  `upper_2.5ci` = c(0.7, 0.3), BetweenWithin = c("Within", "Between"),
                  check.names = FALSE, stringsAsFactors = FALSE)
  p <- mplus_param_block(u)
  expect_equal(p$group, c("Within", "Between"))
  expect_equal(p$se, c(0.1, 0.05))
  expect_equal(p$ci_lower, c(0.3, 0.1))
  expect_true(all(is.na(p$p)))     # one-tailed posterior p-values are not used
})
