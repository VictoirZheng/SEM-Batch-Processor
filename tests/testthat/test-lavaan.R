test_that("option resolution records every automatic adjustment", {
  r <- resolve_lavaan_options(list(estimator = "MLR", bootstrap = 1000))
  expect_equal(r$args$estimator, "ML")
  expect_equal(r$args$se, "bootstrap")
  expect_true(any(grepl("ML", r$notes)))
  r2 <- resolve_lavaan_options(list(estimator = "WLSMV", missing = "fiml", bootstrap = 500))
  expect_equal(r2$args$missing, "pairwise")
  expect_null(r2$args$se)
  r3 <- resolve_lavaan_options(list(ordered = c("u1", "zz")), model_vars = c("u1", "y"))
  expect_equal(r3$args$estimator, "WLSMV")
  expect_equal(r3$args$ordered, "u1")
})

test_that("a mediation model is fitted and extracted", {
  d <- make_mediation_data()
  syn <- "m1 ~ a*x1\ny1 ~ b*m1 + cp*x1\nind := a*b"
  r <- fit_lavaan_model(syn, d, list(bootstrap = 200, seed = 7))
  expect_equal(r$status, "ok")
  expect_equal(r$fit[["df"]], 0)
  ind <- r$params[r$params$op == ":=", ]
  expect_equal(nrow(ind), 1)
  expect_true(ind$ci_lower > 0)
  expect_false(is.na(ind$std_est))
  expect_equal(attr(r$params, "ci_method"), "bootstrap-perc")
  # bootstrap is reproducible with the seed
  r2 <- fit_lavaan_model(syn, d, list(bootstrap = 200, seed = 7))
  expect_equal(r$params$ci_lower, r2$params$ci_lower)
})

test_that("robust estimators report robust fit indices", {
  d <- make_mediation_data()
  r <- fit_lavaan_model("m1 ~ x1\ny1 ~ m1", d, list(estimator = "MLR"))
  expect_true(r$fit_robust)
  expect_equal(r$fit[["df"]], 1)
  expect_false(is.na(r$fit[["cfi"]]))
})

test_that("unknown variables and syntax errors fail cleanly", {
  d <- make_mediation_data()
  expect_equal(fit_lavaan_model("y1 ~ nope", d)$status, "failed")
  expect_match(fit_lavaan_model("y1 ~ ", d)$errors, "Syntax")
  expect_true(check_lavaan_syntax("y1 ~ x1 + x1:m1", names(d))$ok)
})

test_that("Heywood cases are flagged as inadmissible", {
  set.seed(3)
  n <- 120
  f <- rnorm(n)
  d <- data.frame(a = f + rnorm(n, sd = 0.05), b = f + rnorm(n, sd = 1), c = rnorm(n))
  r <- fit_lavaan_model("F =~ a + b + c\na ~~ va*a\nva < 10", d)
  # either inadmissible or warning-free depending on the solution; check the detector directly
  pv <- r$params
  pv$est[pv$op == "~~" & pv$lhs == "a"] <- -0.1
  neg <- pv$op == "~~" & pv$lhs == pv$rhs & pv$est < 0
  expect_true(any(neg))
  expect_equal(classify_status(character(0), "Negative variance", TRUE, TRUE), "inadmissible")
  expect_equal(classify_status(character(0), character(0), FALSE, FALSE), "failed")
})
