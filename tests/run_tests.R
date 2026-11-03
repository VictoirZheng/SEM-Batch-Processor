# Run the test suite from the project folder:  Rscript tests/run_tests.R
if (.Platform$OS.type != "windows" && !isTRUE(l10n_info()$`UTF-8`)) {
  invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
}
testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
