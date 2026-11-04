# lavaan engine: option handling, syntax checks, fitting and extraction.
# Sourced by worker processes together with utils.R.

LAVAAN_ESTIMATORS <- c("ML", "MLR", "MLM", "MLMV", "WLSMV", "ULSMV")
ROBUST_ESTIMATORS <- c("MLR", "MLM", "MLMV", "MLMVS", "WLSMV", "ULSMV", "WLSM", "ULSM")
BOOT_CI_TYPES <- c("perc", "bca.simple", "bca", "norm")

#' Default estimation options
lavaan_default_options <- function() {
  list(
    estimator = "ML",
    missing = "fiml",         # listwise / fiml / fiml.x
    bootstrap = 0,            # 0 = no bootstrap; otherwise number of draws
    boot_ci = "perc",
    seed = 12345,
    std_lv = FALSE,           # identification: marker variable (FALSE) or unit variance (TRUE)
    ordered = character(0),
    level = 0.95
  )
}

#' Normalise options and record every adjustment made
#'
#' @return list(args = arguments for lavaan::sem(), opts = final options,
#'   notes = character vector describing automatic adjustments)
resolve_lavaan_options <- function(opts, model_vars = NULL) {
  o <- utils::modifyList(lavaan_default_options(), opts %||% list())
  notes <- character(0)
  ordered <- intersect(o$ordered %||% character(0), model_vars %||% o$ordered)

  est <- toupper(o$estimator %||% "ML")
  if (!est %in% LAVAAN_ESTIMATORS) est <- "ML"
  if (length(ordered) > 0 && !est %in% c("WLSMV", "ULSMV")) {
    notes <- c(notes, sprintf("Ordered-categorical variables (%s) present: estimator switched from %s to WLSMV.",
                              paste(ordered, collapse = ", "), est))
    est <- "WLSMV"
  }

  B <- as.integer(o$bootstrap %||% 0)
  if (is.na(B) || B < 0) B <- 0L
  if (B > 0 && est != "ML") {
    if (est %in% c("WLSMV", "ULSMV")) {
      notes <- c(notes, "lavaan does not support bootstrap with WLSMV/ULSMV: bootstrap disabled, robust standard errors used.")
      B <- 0L
    } else {
      notes <- c(notes, sprintf("lavaan bootstraps only under ML: estimator %s replaced by ML (bootstrap SEs and CIs do not assume normality; the chi-square test is the normal-theory one).", est))
      est <- "ML"
    }
  }

  missing <- o$missing %||% "fiml"
  if (!missing %in% c("listwise", "fiml", "fiml.x")) missing <- "fiml"
  if (est %in% c("WLSMV", "ULSMV") && missing != "listwise") {
    notes <- c(notes, "FIML is not available with WLSMV/ULSMV: pairwise deletion used instead.")
    missing <- "pairwise"
  }
  if (est %in% c("MLM", "MLMV") && missing != "listwise") {
    notes <- c(notes, sprintf("%s requires complete data: listwise deletion used (choose MLR for robust estimation with FIML).", est))
    missing <- "listwise"
  }

  args <- list(estimator = est, missing = missing, std.lv = isTRUE(o$std_lv))
  if (length(ordered) > 0) args$ordered <- ordered
  if (B > 0) {
    args$se <- "bootstrap"
    args$bootstrap <- B
    args$iseed <- as.integer(o$seed %||% 12345)
  }

  o$estimator <- est
  o$missing <- missing
  o$bootstrap <- B
  o$ordered <- ordered
  if (!o$boot_ci %in% BOOT_CI_TYPES) o$boot_ci <- "perc"
  list(args = args, opts = o, notes = notes)
}

# Functions that may appear in lavaan model syntax: lavaan's modifiers and
# arithmetic used in defined parameters (:=) and constraints.
LAVAAN_ALLOWED_CALLS <- c(
  "c", "start", "label", "equal", "upper", "lower", "rv", "efa",
  "exp", "log", "log10", "log2", "log1p", "expm1", "sqrt", "abs", "sign",
  "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh",
  "asinh", "acosh", "atanh", "pnorm", "qnorm", "dnorm", "plogis", "qlogis",
  "pt", "qt", "pchisq", "qchisq", "min", "max", "pmin", "pmax", "sum", "prod",
  "mean", "ifelse", "round", "floor", "ceiling"
)

#' Reject syntax that could execute arbitrary R code
#'
#' lavaan evaluates modifiers (e.g. start(...)) and the right-hand side of
#' defined parameters and constraints as R expressions. On a shared server
#' this would let any visitor run code, so only lavaan modifiers and
#' arithmetic functions are allowed, and characters that have no meaning in
#' lavaan syntax are rejected.
#' @return character vector of problems (empty when the syntax is safe)
lavaan_syntax_safety <- function(syntax) {
  code <- gsub("[#!][^\n]*", "", syntax)
  problems <- character(0)
  calls <- regmatches(code, gregexpr("[A-Za-z.][A-Za-z0-9._]*(?=\\s*\\()", code, perl = TRUE))[[1]]
  bad <- setdiff(unique(calls), LAVAAN_ALLOWED_CALLS)
  if (length(bad) > 0) {
    problems <- c(problems, sprintf("Function(s) not allowed in model syntax: %s",
                                    paste0(bad, "()", collapse = ", ")))
  }
  if (grepl("[`$@{}]|\\[|\\]|::|<-|->|%", code, perl = TRUE)) {
    problems <- c(problems, "The syntax contains characters that are not part of lavaan syntax (` $ @ [ ] { } :: <- -> %). Write 'a < -1' with spaces.")
  }
  problems
}

#' Syntax and variable check for a lavaan model
#' @return list(ok, errors, observed, latent)
check_lavaan_syntax <- function(syntax, data_names = NULL) {
  out <- list(ok = FALSE, errors = character(0), observed = character(0),
              latent = character(0))
  unsafe <- lavaan_syntax_safety(syntax)
  if (length(unsafe) > 0) {
    out$errors <- unsafe
    return(out)
  }
  pt <- tryCatch(lavaan::lavaanify(syntax, warn = FALSE),
                 error = function(e) e)
  if (inherits(pt, "error")) {
    out$errors <- paste("Syntax error:", conditionMessage(pt))
    return(out)
  }
  out$observed <- lavaan::lavNames(pt, "ov")
  out$latent <- lavaan::lavNames(pt, "lv")
  if (!is.null(data_names)) {
    # interaction terms such as x:w are created by lavaan from x and w
    obs <- unique(unlist(strsplit(out$observed, ":", fixed = TRUE)))
    unknown <- setdiff(obs, data_names)
    if (length(unknown) > 0) {
      out$errors <- c(out$errors, paste("Variables not found in the data:",
                                        paste(unknown, collapse = ", ")))
    }
  }
  out$ok <- length(out$errors) == 0
  out
}

#' Fit one lavaan model and extract everything needed downstream
#'
#' @param syntax lavaan model syntax (placeholders already filled)
#' @param data data.frame
#' @param opts options list (see lavaan_default_options)
#' @param id model id
#' @param fit_file optional path; the lavaan object is saved there (RDS)
#'   so that path diagrams can be drawn later
#' @return unified model result (see new_model_result)
fit_lavaan_model <- function(syntax, data, opts = list(), id = "M001", fit_file = NULL) {
  t0 <- Sys.time()
  res <- new_model_result(id, "lavaan")
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    res$errors <- "Package 'lavaan' is not installed"
    return(res)
  }

  chk <- check_lavaan_syntax(syntax, names(data))
  if (!chk$ok) {
    res$errors <- chk$errors
    res$elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    return(res)
  }
  model_vars <- unique(unlist(strsplit(chk$observed, ":", fixed = TRUE)))
  resolved <- resolve_lavaan_options(opts, model_vars)
  res$notes <- resolved$notes
  res$lavaan_args <- resolved$args   # exactly what was passed to sem(), for reproduce.R
  o <- resolved$opts

  warns <- character(0)
  fit <- withCallingHandlers(
    tryCatch(
      do.call("sem", c(list(model = syntax, data = data), resolved$args),
              envir = asNamespace("lavaan")),
      error = function(e) e
    ),
    warning = function(w) {
      warns <<- c(warns, clean_lavaan_message(conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )

  if (inherits(fit, "error")) {
    res$errors <- clean_lavaan_message(conditionMessage(fit))
    res$warnings <- unique(warns)
    res$elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    return(res)
  }

  converged <- isTRUE(tryCatch(lavaan::lavInspect(fit, "converged"), error = function(e) FALSE))
  if (!converged) res$errors <- c(res$errors, "Model did not converge")

  # --- fit indices --------------------------------------------------------
  fm <- tryCatch(lavaan::fitMeasures(fit), error = function(e) NULL)
  robust <- o$estimator %in% ROBUST_ESTIMATORS
  res$fit_robust <- robust
  # For robust estimators use the robust version, then the scaled version,
  # then the standard one (availability differs between lavaan versions).
  pick <- function(std, alt = character(0)) {
    if (is.null(fm)) return(NA_real_)
    if (robust) {
      for (a in alt) if (a %in% names(fm) && !is.na(fm[[a]])) return(unname(fm[[a]]))
    }
    if (std %in% names(fm)) unname(fm[[std]]) else NA_real_
  }
  res$fit[] <- c(
    n = sum(lavaan::lavInspect(fit, "nobs")),
    npar = pick("npar"),
    chisq = pick("chisq", "chisq.scaled"),
    df = pick("df", "df.scaled"),
    pvalue = pick("pvalue", "pvalue.scaled"),
    cfi = pick("cfi", c("cfi.robust", "cfi.scaled")),
    tli = pick("tli", c("tli.robust", "tli.scaled")),
    rmsea = pick("rmsea", c("rmsea.robust", "rmsea.scaled")),
    rmsea_lower = pick("rmsea.ci.lower", c("rmsea.ci.lower.robust", "rmsea.ci.lower.scaled")),
    rmsea_upper = pick("rmsea.ci.upper", c("rmsea.ci.upper.robust", "rmsea.ci.upper.scaled")),
    srmr = pick("srmr"),
    aic = pick("aic"),
    bic = pick("bic")
  )[FIT_FIELDS]

  # --- parameters ---------------------------------------------------------
  if (converged) {
    res$params <- withCallingHandlers(
      tryCatch(extract_lavaan_params(fit, o),
               error = function(e) {
                 warns <<- c(warns, paste("Parameter extraction:", conditionMessage(e)))
                 empty_param_table()
               }),
      warning = function(w) {
        warns <<- c(warns, clean_lavaan_message(conditionMessage(w)))
        invokeRestart("muffleWarning")
      })
    res$notes <- c(res$notes, attr(res$params, "notes"))
  }

  # --- admissibility ------------------------------------------------------
  inadmissible <- FALSE
  if (converged && nrow(res$params) > 0) {
    pv <- res$params
    neg <- pv$op == "~~" & pv$lhs == pv$rhs & !is.na(pv$est) & pv$est < 0
    if (any(neg)) {
      inadmissible <- TRUE
      warns <- c(warns, paste("Negative variance estimate(s) (Heywood case):",
                              paste(unique(pv$lhs[neg]), collapse = ", ")))
    }
    big_std <- pv$op %in% c("=~", "~~") & !is.na(pv$std_est) & abs(pv$std_est) > 1 &
      !(pv$op == "~~" & pv$lhs == pv$rhs)
    if (any(big_std)) {
      inadmissible <- TRUE
      warns <- c(warns, "Standardized loading or correlation with |value| > 1")
    }
  }
  if (converged && o$bootstrap > 0) {
    boot <- tryCatch(lavaan::lavInspect(fit, "boot"), error = function(e) NULL)
    if (!is.null(boot)) {
      n_ok <- sum(stats::complete.cases(boot))
      res$notes <- c(res$notes, sprintf("Bootstrap: %d of %d draws converged.", n_ok, o$bootstrap))
      if (n_ok < 0.9 * o$bootstrap) {
        warns <- c(warns, sprintf("Only %d of %d bootstrap draws converged", n_ok, o$bootstrap))
      }
    }
  }
  if (any(is_inadmissible_warning(warns))) inadmissible <- TRUE
  if (converged) {
    n_dropped <- nrow(data) - res$fit[["n"]]
    if (!is.na(n_dropped) && n_dropped > 0) {
      res$notes <- c(res$notes, sprintf("%d case(s) excluded (missing data handling: %s).",
                                        as.integer(n_dropped), o$missing))
    }
    ratio <- res$fit[["n"]] / res$fit[["npar"]]
    if (!is.na(ratio) && ratio < 5) {
      res$notes <- c(res$notes, sprintf("Only %.1f cases per free parameter (N = %d, %d parameters): estimates and fit indices may be unstable in samples this small relative to model size (a common rule of thumb asks for at least 5-10 cases per parameter).",
                                        ratio, as.integer(res$fit[["n"]]), as.integer(res$fit[["npar"]])))
    }
  }

  res$warnings <- unique(warns)
  res$status <- classify_status(res$errors, res$warnings, converged, inadmissible)

  res$output_text <- tryCatch(
    utils::capture.output(lavaan::summary(fit, fit.measures = TRUE, standardized = TRUE,
                                          rsquare = TRUE, ci = TRUE)),
    error = function(e) paste("summary() failed:", conditionMessage(e)))

  if (!is.null(fit_file)) {
    ok <- tryCatch({ saveRDS(fit, fit_file); TRUE }, error = function(e) FALSE)
    if (ok) res$fit_file <- fit_file
  }
  res$elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  res
}

#' Does a lavaan warning indicate an inadmissible solution?
#'
#' Non-positive-definite residual or latent covariance matrices and failed
#' standard errors do. lavaan's vcov message ("does not appear to be positive
#' definite ... close to zero") is often harmless (e.g. with equality
#' constraints) and is therefore only a warning.
is_inadmissible_warning <- function(w) {
  grepl("is not positive[- ]definite|could not compute standard errors", w, ignore.case = TRUE)
}

#' lavaan warnings start with "lavaan WARNING:" and contain hard line breaks
clean_lavaan_message <- function(x) {
  x <- gsub("lavaan (WARNING|ERROR|NOTE):?\\s*", "", x)
  x <- gsub("\\s*\n\\s*", " ", x)
  trimws(x)
}

#' Unified parameter table from a fitted lavaan model
extract_lavaan_params <- function(fit, o) {
  boot <- o$bootstrap > 0
  ci_type <- if (boot) o$boot_ci else "norm"
  notes <- character(0)
  get_pe <- function(type) lavaan::parameterEstimates(fit, ci = TRUE, level = o$level,
                                                      boot.ci.type = type,
                                                      rsquare = TRUE, standardized = FALSE)
  pe <- tryCatch(get_pe(ci_type), error = function(e) {
    # BCa needs more successful bootstrap draws than observations; fall back
    # to the bias-corrected interval (BCa without the acceleration term).
    if (!identical(ci_type, "bca")) stop(e)
    notes <<- c(notes, sprintf("BCa intervals were not available (%s): bias-corrected intervals (bca.simple) are reported instead. Use more bootstrap draws than cases for BCa.",
                               clean_lavaan_message(conditionMessage(e))))
    ci_type <<- "bca.simple"
    get_pe("bca.simple")
  })
  ss <- tryCatch(lavaan::standardizedSolution(fit, type = "std.all", level = o$level),
                 error = function(e) NULL)

  grp <- if ("group" %in% names(pe)) as.character(pe$group) else rep("", nrow(pe))
  if (length(unique(grp)) <= 1) grp <- rep("", nrow(pe))
  out <- data.frame(
    group = grp,
    lhs = pe$lhs, op = pe$op, rhs = pe$rhs,
    label = if ("label" %in% names(pe)) pe$label else "",
    est = pe$est, se = pe$se, z = pe$z, p = pe$pvalue,
    ci_lower = pe$ci.lower, ci_upper = pe$ci.upper,
    std_est = NA_real_, std_se = NA_real_, std_p = NA_real_,
    std_ci_lower = NA_real_, std_ci_upper = NA_real_,
    stringsAsFactors = FALSE
  )

  # Fixed parameters have no sampling variability
  fixed <- !is.na(out$se) & out$se == 0
  out$z[fixed] <- NA
  out$p[fixed] <- NA

  if (!is.null(ss)) {
    sgrp <- if ("group" %in% names(ss)) as.character(ss$group) else rep("", nrow(ss))
    if (length(unique(sgrp)) <= 1) sgrp <- rep("", nrow(ss))
    key_pe <- paste(grp, pe$lhs, pe$op, pe$rhs, sep = "\r")
    key_ss <- paste(sgrp, ss$lhs, ss$op, ss$rhs, sep = "\r")
    m <- match(key_pe, key_ss)
    out$std_est <- ss$est.std[m]
    out$std_se <- ss$se[m]
    out$std_p <- ss$pvalue[m]
    out$std_ci_lower <- ss$ci.lower[m]
    out$std_ci_upper <- ss$ci.upper[m]
  }
  # R-square rows are already standardized quantities
  r2 <- out$op == "r2"
  out$std_est[r2] <- out$est[r2]

  attr(out, "ci_method") <- if (boot) paste0("bootstrap-", ci_type) else "wald"
  attr(out, "notes") <- notes
  out
}
