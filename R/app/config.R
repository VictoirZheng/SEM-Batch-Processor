# Application configuration and run mode.

APP_VERSION <- "3.0.0"

load_app_config <- function(dir = "config") {
  read <- function(f) {
    p <- file.path(dir, f)
    if (!file.exists(p) || !requireNamespace("yaml", quietly = TRUE)) return(list())
    tryCatch(yaml::read_yaml(p), error = function(e) {
      warning(sprintf("Could not read %s: %s", p, conditionMessage(e)))
      list()
    })
  }
  cfg <- utils::modifyList(read("default.yaml"), read("local.yaml"))

  mode <- Sys.getenv("SEMBP_MODE", cfg$app$mode %||% "auto")
  if (!mode %in% c("online", "local")) {
    hosted <- Sys.getenv("R_CONFIG_ACTIVE") %in% c("shinyapps", "rsconnect") ||
      nzchar(Sys.getenv("CONNECT_SERVER")) || nzchar(Sys.getenv("SHINYAPPS_USER"))
    mode <- if (hosted) "online" else "local"
  }
  cfg$mode <- mode
  cfg$language <- normalize_lang(Sys.getenv("SEMBP_LANG", cfg$app$language %||% "en"))
  cfg$limit <- cfg$limits[[mode]] %||% list(max_models = 300, max_bootstrap = 5000,
                                            max_workers = 2, max_upload_mb = 30,
                                            default_timeout_s = 600)
  cores <- tryCatch(parallel::detectCores(), error = function(e) 2)
  if (is.na(cores)) cores <- 2
  cfg$limit$max_workers <- max(1, min(cfg$limit$max_workers %||% 2, cores))

  cfg$mplus_path <- if (mode == "online") "" else
    detect_mplus(c(Sys.getenv("SEMBP_MPLUS_PATH"), cfg$mplus$executable_path %||% character(0)))
  cfg$mplus_automation <- requireNamespace("MplusAutomation", quietly = TRUE)
  cfg$version <- APP_VERSION
  cfg
}

normalize_lang <- function(x) {
  x <- tolower(x %||% "en")
  if (startsWith(x, "zh") || startsWith(x, "cn")) "zh" else "en"
}

#' Is the Mplus engine usable in this deployment?
mplus_enabled <- function(cfg) {
  identical(cfg$mode, "local") && nzchar(cfg$mplus_path %||% "")
}
