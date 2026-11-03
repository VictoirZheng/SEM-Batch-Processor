test_that("no source file starts with a byte-order mark (broke v2 on Linux)", {
  files <- list.files(PROJECT_ROOT, pattern = "\\.(R|lav|inp|yaml|js|css)$", recursive = TRUE, full.names = TRUE)
  files <- files[!grepl("/tests/", files)]
  bom <- vapply(files, function(f) {
    r <- readBin(f, "raw", 3)
    length(r) == 3 && identical(r, as.raw(c(0xef, 0xbb, 0xbf)))
  }, logical(1))
  expect_equal(names(bom)[bom], character(0))
})

test_that("every translation key used in the app exists in both languages", {
  src <- unlist(lapply(list.files(file.path(PROJECT_ROOT, "R", "app"), full.names = TRUE), readLines, encoding = "UTF-8"))
  keys <- unique(unlist(regmatches(src, gregexpr('(?<![A-Za-z_])tr\\("[a-z0-9_]+"', src, perl = TRUE))))
  keys <- sub('tr\\("', "", sub('"$', "", keys))
  dyn <- c(paste0("st_", c("ok", "warning", "inadmissible", "failed", "cancelled", "pending")),
           paste0("fj_", c("good", "acceptable", "poor", "saturated")),
           paste0("type_", PH_TYPES))
  missing <- setdiff(c(keys, dyn), names(I18N))
  expect_equal(missing, character(0))
  incomplete <- names(I18N)[!vapply(I18N, function(e) all(c("en", "zh") %in% names(e)), logical(1))]
  expect_equal(incomplete, character(0))
  # sprintf placeholders agree between languages
  n_args <- function(s) length(regmatches(s, gregexpr("%[0-9$]*[sdf]", s))[[1]])
  bad <- names(I18N)[vapply(I18N, function(e) n_args(e$en) != n_args(e$zh), logical(1))]
  expect_equal(bad, character(0))
})

test_that("configuration detects the run mode", {
  withr_env <- function(vars, code) {
    old <- Sys.getenv(names(vars), unset = NA)
    do.call(Sys.setenv, as.list(vars))
    on.exit(for (n in names(old)) if (is.na(old[[n]])) Sys.unsetenv(n) else do.call(Sys.setenv, stats::setNames(list(old[[n]]), n)))
    code
  }
  cfg_dir <- file.path(PROJECT_ROOT, "config")
  expect_equal(withr_env(c(SEMBP_MODE = "online"), load_app_config(cfg_dir)$mode), "online")
  expect_equal(withr_env(c(SEMBP_MODE = "online"), load_app_config(cfg_dir)$mplus_path), "")
  expect_equal(withr_env(c(SEMBP_MODE = "auto", R_CONFIG_ACTIVE = "shinyapps"), load_app_config(cfg_dir)$mode), "online")
  expect_equal(withr_env(c(SEMBP_MODE = "local"), load_app_config(cfg_dir)$limit$max_models), 5000)
})
