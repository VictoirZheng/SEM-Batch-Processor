# Parsing of real Mplus output files. MplusAutomation ships example outputs
# from the Mplus User's Guide; these tests run wherever it is installed
# (e.g. in the GitHub Actions workflow).

# In CI (SEMBP_REQUIRE_MPLUS_TESTS=true) a missing package or example file
# is an error, so that the parser can never go untested silently.
require_mplus_tests <- identical(Sys.getenv("SEMBP_REQUIRE_MPLUS_TESTS"), "true")

mplus_example <- function(name) {
  if (!requireNamespace("MplusAutomation", quietly = TRUE)) {
    if (require_mplus_tests) stop("MplusAutomation is required for these tests")
    skip("MplusAutomation not installed")
  }
  f <- system.file("extdata", name, package = "MplusAutomation")
  if (!nzchar(f)) {
    if (require_mplus_tests) stop("Example output not found in MplusAutomation: ", name)
    skip(paste("example output not available:", name))
  }
  f
}

test_that("bootstrapped path model with indirect effects (UG ex3.16) is parsed", {
  f <- mplus_example("ex3.16.out")
  nm <- mplus_name_map(c("y1", "y2", "y3", "x1", "x2", "x3"))
  r <- parse_mplus_output(f, "M001", nm)
  expect_equal(r$status, "ok")
  expect_equal(r$fit[["n"]], 500)
  expect_equal(r$fit[["npar"]], 15)
  expect_equal(r$fit[["cfi"]], 1.000)
  expect_equal(r$fit[["srmr"]], 0.001)
  p <- r$params
  # names are mapped back to the original (lower-case) data names
  path <- p[p$op == "~" & p$lhs == "y1" & p$rhs == "x1", ]
  expect_equal(path$est, 0.992)
  expect_equal(c(path$ci_lower, path$ci_upper), c(0.907, 1.075))
  ind <- p[p$op == "ind", ]
  expect_equal(ind$label, c("x1 -> y1 -> y3", "x1 -> y2 -> y3"))
  expect_equal(ind$est, c(0.503, 2.188))
  expect_equal(ind$ci_lower, c(0.445, 2.054))
  expect_equal(ind$ci_upper, c(0.558, 2.310))
  tot <- p[p$op == "total_ind", ]
  expect_equal(c(tot$est, tot$ci_lower, tot$ci_upper), c(2.691, 2.567, 2.813))
  # the effect is recognised at template level and enters the focal table
  man <- data.frame(id = "M001", name = "m", syntax = "", template_id = "T1",
                    "{{X}}" = "x1", "{{M}}" = "y1", "{{Y}}" = "y3", check.names = FALSE)
  focal <- build_focal_table(list(M001 = r), man, types = "indirect")
  expect_true("{{X}} -> {{M}} -> {{Y}}" %in% focal$effect_key)
})

test_that("latent indirect effects (UG ex5.12) are parsed", {
  f <- mplus_example("ex5.12.out")
  r <- parse_mplus_output(f, "M001", mplus_name_map(sprintf("y%d", 1:12)))
  expect_equal(r$status, "ok")
  expect_true(any(r$params$op == "=~" & r$params$lhs == "F1" & r$params$rhs == "y1"))
  expect_true("F1 -> F3 -> F4" %in% r$params$label[r$params$op == "ind"])
})

test_that("other example outputs parse without errors", {
  dir <- dirname(mplus_example("ex3.16.out"))
  files <- list.files(dir, pattern = "^ex[0-9.]+[a-z]?\\.out$", full.names = TRUE)
  skip_if(length(files) == 0, "no example outputs")
  for (f in files) {
    r <- parse_mplus_output(f, "M001", NULL)
    expect_true(r$status %in% c("ok", "warning", "inadmissible", "failed"), info = basename(f))
    if (r$status != "failed") expect_gt(nrow(r$params), 0)
  }
})

test_that("Bayesian output uses posterior SD and credibility intervals, not one-tailed p (UG ex3.18)", {
  f <- mplus_example("ex3.18.out")
  r <- parse_mplus_output(f, "M001", mplus_name_map(c("y", "m", "x", "z")))
  expect_true(r$status %in% c("ok", "warning"))
  b <- r$params[r$params$op == "~" & r$params$lhs == "y" & r$params$rhs == "m", ]
  expect_equal(b$est, 0.518)
  expect_equal(b$se, 0.057)
  expect_equal(c(b$ci_lower, b$ci_upper), c(0.406, 0.630))
  expect_true(is.na(b$p))
})

test_that("multilevel parameters keep their level (UG ex9.2c)", {
  f <- mplus_example("ex9.2c.out")
  r <- parse_mplus_output(f, "M001", NULL)
  p <- r$params
  bw <- p[p$op == "~" & p$lhs == "Y" & p$rhs == "W", ]
  expect_equal(bw$group, "Between")
  expect_equal(bw$est, 1.186)
})
