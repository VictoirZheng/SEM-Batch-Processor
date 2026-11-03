# Core utilities shared by the app and by background worker processes.
# This file must stay dependency-free (base R only) because worker
# processes source it directly.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Return x if it is a single non-NA value, otherwise `default`
scalar_or <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0) return(default)
  x <- x[[1]]
  if (length(x) == 0 || (is.atomic(x) && is.na(x))) default else x
}

num_or_na <- function(x) {
  v <- suppressWarnings(as.numeric(scalar_or(x, NA_real_)))
  if (length(v) == 0) NA_real_ else v
}

#' Format a number for display; NA becomes an em dash
fmt_num <- function(x, digits = 3) {
  x <- suppressWarnings(as.numeric(x))
  out <- formatC(x, format = "f", digits = digits)
  out[is.na(x)] <- "—"
  out
}

#' APA-style p-value formatting
fmt_p <- function(p) {
  p <- suppressWarnings(as.numeric(p))
  out <- ifelse(p < 0.001, "< .001", sub("^0", "", formatC(p, format = "f", digits = 3)))
  out[is.na(p)] <- "—"
  out
}

#' Zero-padded model ids: M001, M002, ...
make_model_ids <- function(n, start = 1) {
  if (n <= 0) return(character(0))
  idx <- seq.int(start, length.out = n)
  width <- max(3, nchar(as.character(max(idx))))
  sprintf(paste0("M%0", width, "d"), idx)
}

#' Escape a string for literal use inside a regular expression
regex_escape <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

#' Write an RDS file atomically (write to temp then rename) so that a
#' polling reader never sees a half-written file.
save_rds_atomic <- function(object, path) {
  tmp <- paste0(path, ".tmp", Sys.getpid())
  saveRDS(object, tmp)
  ok <- file.rename(tmp, path)
  if (!ok) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(path)
}

#' Remove a UTF-8 byte-order mark from the start of a string
strip_bom <- function(x) {
  if (length(x) == 0) return(x)
  bom <- intToUtf8(0xFEFF)
  has <- !is.na(x) & startsWith(x, bom)
  x[has] <- substring(x[has], 2)
  x
}

#' Read a text file as a single string (UTF-8, BOM removed)
read_text_file <- function(path) {
  txt <- readLines(path, warn = FALSE, encoding = "UTF-8")
  strip_bom(paste(txt, collapse = "\n"))
}

#' Create an empty unified parameter table
empty_param_table <- function() {
  data.frame(
    group = character(0), lhs = character(0), op = character(0),
    rhs = character(0), label = character(0),
    est = numeric(0), se = numeric(0), z = numeric(0), p = numeric(0),
    ci_lower = numeric(0), ci_upper = numeric(0),
    std_est = numeric(0), std_se = numeric(0), std_p = numeric(0),
    std_ci_lower = numeric(0), std_ci_upper = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Names of the fit indices kept for every model (both engines)
FIT_FIELDS <- c("n", "npar", "chisq", "df", "pvalue", "cfi", "tli",
                "rmsea", "rmsea_lower", "rmsea_upper", "srmr", "aic", "bic")

empty_fit <- function() {
  stats::setNames(rep(NA_real_, length(FIT_FIELDS)), FIT_FIELDS)
}

#' Skeleton of a single-model result (identical for lavaan and Mplus)
new_model_result <- function(id, engine) {
  list(
    id = id,
    engine = engine,
    status = "failed",        # ok / warning / inadmissible / failed
    errors = character(0),
    warnings = character(0),
    notes = character(0),
    fit = empty_fit(),
    fit_robust = FALSE,
    params = empty_param_table(),
    output_text = character(0),
    elapsed = NA_real_,
    fit_file = NULL
  )
}

#' Decide the overall status of a model from its diagnostics
classify_status <- function(errors, warnings, converged, inadmissible) {
  if (length(errors) > 0 || isFALSE(converged)) return("failed")
  if (isTRUE(inadmissible)) return("inadmissible")
  if (length(warnings) > 0) return("warning")
  "ok"
}
