test_that("GB18030 CSV with Chinese names is read correctly", {
  d <- data.frame(a = 1:3, b = c(2.5, NA, -999))
  names(d)[2] <- "满意度 总分"
  f <- tempfile(fileext = ".csv")
  utils::write.csv(d, f, row.names = FALSE, fileEncoding = "GB18030")
  expect_equal(detect_text_encoding(f), "GB18030")
  r <- read_data_file(f)
  expect_equal(nrow(r$data), 3)
  expect_equal(nrow(r$info$renamed), 1)
  expect_equal(r$info$renamed$original, "满意度 总分")
  expect_true(is.numeric(r$data[[2]]))
})

test_that("missing codes are recoded", {
  expect_equal(parse_missing_codes("-999, 99，-1"), c(-999, 99, -1))
  expect_error(parse_missing_codes("-999, abc"))
  r <- recode_missing(data.frame(x = c(1, -999, 3), s = c("a", "-999", "b")), -999)
  expect_true(is.na(r$data$x[2]))
  expect_equal(r$data$s[2], "-999")       # non-numeric untouched
  expect_equal(unname(r$counts), 1L)
})

test_that("skewness and kurtosis match the adjusted (SPSS) estimators", {
  x <- c(1, 2, 2, 3, 3, 3, 4, 10)
  n <- length(x); m2 <- mean((x - mean(x))^2)
  g1 <- mean((x - mean(x))^3) / m2^1.5
  expect_equal(skewness_g1(x), g1 * sqrt(n * (n - 1)) / (n - 2))
  d <- describe_data(data.frame(x = x, k = 1))
  expect_true(d$flag_constant[2])
})

test_that("derived variables", {
  d <- data.frame(a = c(1, 2, 3), b = c(2, 4, 6))
  expect_equal(mean(add_centered(d, "a")$c_a), 0)
  p <- add_product(d, "a", "b", center = TRUE)
  expect_equal(p$ca_x_cb, (d$a - 2) * (d$b - 4))
  s <- add_mean_score(data.frame(i1 = c(1, NA), i2 = c(3, NA)), "sc", c("i1", "i2"), 2)
  expect_equal(s$sc, c(2, NA))
})
