# Cross-model aggregation: fit table, focal effects, multiple-comparison
# correction and effect summaries.

FOCAL_TYPES <- c("defined", "indirect", "total", "regression", "loading", "covariance")
P_ADJUST_METHODS <- c("holm", "BH", "bonferroni", "none")

OP_SYMBOL <- c("~" = "←", "=~" = "=~", "~~" = "↔", ":=" = ":=",
               ind = "ind", total_ind = "total ind.", total = "total", direct = "direct")

#' Rows of a parameter table that belong to the requested focal types
focal_rows <- function(params, types = c("defined", "indirect"), labels = NULL) {
  if (is.null(params) || nrow(params) == 0) return(logical(0))
  op <- params$op
  keep <- rep(FALSE, nrow(params))
  if ("defined" %in% types) keep <- keep | op == ":="
  if ("indirect" %in% types) keep <- keep | op %in% c("ind", "total_ind")
  if ("total" %in% types) keep <- keep | op == "total"
  if ("regression" %in% types) keep <- keep | op == "~"
  if ("loading" %in% types) keep <- keep | op == "=~"
  if ("covariance" %in% types) keep <- keep | (op == "~~" & params$lhs != params$rhs)
  labels <- labels[nzchar(labels %||% character(0))]
  # case-insensitive: Mplus reports labels and NEW parameters in upper case
  if (length(labels) > 0) keep <- keep & toupper(params$label) %in% toupper(labels)
  keep & !is.na(params$est) & !is.na(params$se) & params$se > 0
}

#' Replace variable names by the placeholder they were assigned to, so that
#' the "same" effect can be recognised across models (e.g. "{{Y}} <- {{X}}").
roleize <- function(x, assignment) {
  assignment <- unlist(assignment)
  if (length(assignment) == 0 || length(x) == 0) return(x)
  lookup <- stats::setNames(paste0("{{", names(assignment), "}}"), assignment)
  vapply(x, function(s) {
    if (is.na(s) || !nzchar(s)) return(s)
    tok <- strsplit(s, " -> ", fixed = TRUE)[[1]]
    tok <- ifelse(tok %in% names(lookup), lookup[tok], tok)
    paste(tok, collapse = " -> ")
  }, character(1), USE.NAMES = FALSE)
}

#' Readable description and template-level key for parameter rows
effect_descriptions <- function(params, assignment = NULL) {
  op <- params$op
  sym <- ifelse(op %in% names(OP_SYMBOL), OP_SYMBOL[op], op)
  grp <- ifelse(nzchar(params$group %||% ""), paste0(" [", params$group, "]"), "")
  is_path <- op %in% c("ind", "total_ind", "total", "direct")
  desc <- ifelse(op == ":=", params$label,
                 ifelse(is_path & op == "ind", params$label,
                        paste(params$lhs, sym, params$rhs)))
  desc <- ifelse(is_path & op != "ind", paste0(sym, ": ", params$rhs, " -> ", params$lhs), desc)
  role_lhs <- roleize(params$lhs, assignment)
  role_rhs <- roleize(params$rhs, assignment)
  key <- ifelse(op == ":=", params$label,
                ifelse(is_path & op == "ind", roleize(params$label, assignment),
                       paste(role_lhs, sym, role_rhs)))
  key <- ifelse(is_path & op != "ind", paste0(sym, ": ", role_rhs, " -> ", role_lhs), key)
  list(desc = paste0(desc, grp), key = paste0(key, grp))
}

#' Fit-index table across models
#'
#' @param results named list of model results (see new_model_result)
#' @param manifest data.frame with columns id, name and one column per
#'   placeholder
build_fit_table <- function(results, manifest) {
  if (length(results) == 0) return(NULL)
  rows <- lapply(manifest$id, function(id) {
    r <- results[[id]]
    f <- if (is.null(r)) empty_fit() else r$fit
    data.frame(
      id = id,
      status = if (is.null(r)) "pending" else r$status,
      n = f[["n"]], npar = f[["npar"]], chisq = f[["chisq"]], df = f[["df"]],
      pvalue = f[["pvalue"]], cfi = f[["cfi"]], tli = f[["tli"]],
      rmsea = f[["rmsea"]], rmsea_lower = f[["rmsea_lower"]], rmsea_upper = f[["rmsea_upper"]],
      srmr = f[["srmr"]], aic = f[["aic"]], bic = f[["bic"]],
      robust = isTRUE(r$fit_robust),
      saturated = isTRUE(!is.na(f[["df"]]) && f[["df"]] == 0),
      n_warnings = length(r$warnings),
      messages = paste(c(r$errors, r$warnings), collapse = " | "),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  extra <- manifest[match(out$id, manifest$id), intersect(c("name", manifest_ph_cols(manifest)), names(manifest)), drop = FALSE]
  rownames(extra) <- NULL
  cbind(out[, "id", drop = FALSE], extra, out[, -1, drop = FALSE])
}

#' Rule-of-thumb description of model fit (Hu & Bentler, 1999). These are
#' heuristics, not tests; saturated models (df = 0) fit perfectly by
#' construction and are reported as such.
fit_judgement <- function(cfi, tli, rmsea, srmr, df) {
  out <- rep("", length(cfi))
  sat <- !is.na(df) & df == 0
  good <- !is.na(cfi) & cfi >= .95 & (is.na(tli) | tli >= .95) &
    (is.na(rmsea) | rmsea <= .06) & (is.na(srmr) | srmr <= .08)
  acceptable <- !good & !is.na(cfi) & cfi >= .90 & (is.na(rmsea) | rmsea <= .08) &
    (is.na(srmr) | srmr <= .10)
  out[good] <- "good"
  out[acceptable] <- "acceptable"
  out[!good & !acceptable & !is.na(cfi)] <- "poor"
  out[sat] <- "saturated"
  out
}

#' Table of focal effects across models with multiple-comparison correction
#'
#' @param results named list of model results
#' @param manifest manifest data.frame (id, name, placeholder columns)
#' @param types focal effect types (see FOCAL_TYPES)
#' @param labels optional character vector; keep only these parameter labels
#' @param method p.adjust method ("holm", "BH", "bonferroni", "none")
#' @param family "all" (one family for the whole batch) or "effect" (a
#'   separate family for each template-level effect)
#' @param include_inadmissible keep inadmissible solutions in the family
#' @param alpha significance level
build_focal_table <- function(results, manifest, types = c("defined", "indirect"),
                              labels = NULL, method = "holm", family = "all",
                              include_inadmissible = FALSE, alpha = 0.05) {
  ph_cols <- manifest_ph_cols(manifest)
  # With several templates in one batch, the same label (e.g. "ind") can mean
  # different effects, so keys are made template-specific.
  multi_template <- "template_id" %in% names(manifest) && length(unique(manifest$template_id)) > 1
  rows <- list()
  for (id in manifest$id) {
    r <- results[[id]]
    if (is.null(r) || r$status == "failed" || nrow(r$params) == 0) next
    if (r$status == "inadmissible" && !include_inadmissible) next
    keep <- focal_rows(r$params, types, labels)
    if (!any(keep)) next
    p <- r$params[keep, , drop = FALSE]
    assignment <- NULL
    if (length(ph_cols) > 0) {
      assignment <- unlist(manifest[manifest$id == id, ph_cols, drop = FALSE][1, ])
      names(assignment) <- ph_name_of(ph_cols)
      assignment <- assignment[!is.na(assignment)]
    }
    d <- effect_descriptions(p, assignment)
    key <- d$key
    if (multi_template) key <- paste0("[", manifest$template_id[manifest$id == id], "] ", key)
    df <- data.frame(id = id, effect_key = key, effect = d$desc,
                     est = p$est, se = p$se, ci_lower = p$ci_lower, ci_upper = p$ci_upper,
                     p = p$p, std_est = p$std_est, std_ci_lower = p$std_ci_lower,
                     std_ci_upper = p$std_ci_upper, status = r$status,
                     stringsAsFactors = FALSE)
    if (length(ph_cols) > 0) {
      for (ph in ph_cols) df[[ph]] <- manifest[[ph]][manifest$id == id]
    }
    rows[[length(rows) + 1]] <- df
  }
  if (length(rows) == 0) return(NULL)
  out <- do.call(rbind, rows)
  method <- if (method %in% P_ADJUST_METHODS) method else "holm"
  if (family == "effect") {
    out$p_adj <- stats::ave(out$p, out$effect_key, FUN = function(p) stats::p.adjust(p, method = method))
    out$family_size <- stats::ave(out$p, out$effect_key, FUN = function(p) sum(!is.na(p)))
  } else {
    out$p_adj <- stats::p.adjust(out$p, method = method)
    out$family_size <- sum(!is.na(out$p))
  }
  out$ci_excludes_0 <- !is.na(out$ci_lower) & !is.na(out$ci_upper) &
    (out$ci_lower > 0 | out$ci_upper < 0)
  out$sig_adj <- !is.na(out$p_adj) & out$p_adj < alpha
  attr(out, "method") <- method
  attr(out, "family") <- family
  attr(out, "alpha") <- alpha
  out[, c("id", ph_cols, setdiff(names(out), c("id", ph_cols)))]
}

#' Summary of each template-level effect across models
summarise_focal <- function(focal) {
  if (is.null(focal) || nrow(focal) == 0) return(NULL)
  sp <- split(focal, focal$effect_key)
  out <- lapply(names(sp), function(k) {
    d <- sp[[k]]
    data.frame(
      effect_key = k,
      n_models = nrow(d),
      median_est = stats::median(d$est, na.rm = TRUE),
      min_est = min(d$est, na.rm = TRUE),
      max_est = max(d$est, na.rm = TRUE),
      median_std = if (all(is.na(d$std_est))) NA_real_ else stats::median(d$std_est, na.rm = TRUE),
      pct_positive = 100 * mean(d$est > 0, na.rm = TRUE),
      pct_ci_excl_0 = 100 * mean(d$ci_excludes_0),
      pct_sig_adj = 100 * mean(d$sig_adj),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

#' Status counts for a batch
status_counts <- function(results, ids) {
  st <- vapply(ids, function(id) if (is.null(results[[id]])) "pending" else results[[id]]$status, character(1))
  c(total = length(ids),
    done = sum(st != "pending"),
    ok = sum(st == "ok"),
    warning = sum(st == "warning"),
    inadmissible = sum(st == "inadmissible"),
    failed = sum(st %in% c("failed", "cancelled", "timeout")))
}

#' Long table of every parameter of every model
build_param_long <- function(results, manifest) {
  rows <- lapply(manifest$id, function(id) {
    r <- results[[id]]
    if (is.null(r) || nrow(r$params) == 0) return(NULL)
    cbind(id = id, r$params, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

#' Rows of the specification panel: one row per value of every placeholder
#' that varies across the models of this effect
spec_panel_rows <- function(d, max_rows = 30) {
  cols <- manifest_ph_cols(d)
  cols <- cols[vapply(cols, function(cn) length(unique(stats::na.omit(d[[cn]]))) > 1, logical(1))]
  rows <- list()
  for (cn in cols) {
    for (v in unique(stats::na.omit(d[[cn]]))) {
      lab <- paste(cn, "=", if (nchar(v) > 24) paste0(substr(v, 1, 22), "…") else v)
      rows[[length(rows) + 1]] <- list(label = lab, x = which(d[[cn]] == v))
    }
  }
  utils::head(rows, max_rows)
}

#' Specification curve of one focal effect across models (Simonsohn,
#' Simmons & Nelson, 2020): estimates sorted by size with their confidence
#' intervals, and below them the specification (variable choice) of each
#' model, so that one can see which choices drive the result.
plot_spec_curve <- function(focal, effect_key, standardized = FALSE, labels = NULL) {
  d <- focal[focal$effect_key == effect_key, , drop = FALSE]
  if (nrow(d) == 0) stop("No estimates for this effect", call. = FALSE)
  y <- if (standardized) d$std_est else d$est
  lo <- if (standardized) d$std_ci_lower else d$ci_lower
  hi <- if (standardized) d$std_ci_upper else d$ci_upper
  ord <- order(y)
  d <- d[ord, , drop = FALSE]
  y <- y[ord]; lo <- lo[ord]; hi <- hi[ord]
  labs <- labels %||% list(x = "Models (sorted by estimate)", y = "Estimate",
                           sig = "Significant after correction", ns = "Not significant",
                           title = effect_key)
  col <- ifelse(d$sig_adj, "#1b6ca8", "#9aa5b1")
  spec <- spec_panel_rows(d)
  left <- max(4.5, if (length(spec) > 0) max(nchar(vapply(spec, `[[`, character(1), "label"))) * 0.42 + 1 else 0)
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  if (length(spec) > 0) {
    graphics::layout(matrix(1:2, ncol = 1), heights = c(3, max(1.2, min(3, 0.25 * length(spec) + 0.6))))
  }
  n <- length(y)
  graphics::par(mar = c(if (length(spec) > 0) 1 else 4.5, left, 3, 1))
  rng <- range(c(lo, hi, y, 0), na.rm = TRUE)
  graphics::plot(seq_len(n), y, xlim = c(0.5, n + 0.5), ylim = rng, pch = 19, col = col,
                 xlab = if (length(spec) > 0) "" else labs$x, ylab = labs$y, main = labs$title,
                 xaxt = if (length(spec) > 0 || n <= 30) "n" else "s")
  if (length(spec) == 0 && n <= 30) graphics::axis(1, at = seq_len(n), labels = d$id, las = 2, cex.axis = 0.7)
  ok <- !is.na(lo) & !is.na(hi)
  graphics::segments(which(ok), lo[ok], which(ok), hi[ok], col = col[ok], lwd = 1.5)
  graphics::abline(h = 0, lty = 2, col = "#c0392b")
  graphics::legend("topleft", legend = c(labs$sig, labs$ns), col = c("#1b6ca8", "#9aa5b1"),
                   pch = 19, bty = "n", cex = 0.85)
  if (length(spec) > 0) {
    nr <- length(spec)
    graphics::par(mar = c(4.5, left, 0.5, 1))
    graphics::plot(NA, xlim = c(0.5, n + 0.5), ylim = c(0.5, nr + 0.5), axes = FALSE,
                   xlab = labs$x, ylab = "")
    graphics::abline(h = seq_len(nr), col = "#eef1f4")
    for (k in seq_len(nr)) {
      xs <- spec[[k]]$x
      graphics::points(xs, rep(nr - k + 1, length(xs)), pch = 15, cex = 0.9, col = col[xs])
    }
    graphics::axis(2, at = rev(seq_len(nr)), labels = vapply(spec, `[[`, character(1), "label"),
                   las = 1, cex.axis = 0.7, tick = FALSE)
    if (n <= 30) graphics::axis(1, at = seq_len(n), labels = d$id, las = 2, cex.axis = 0.7)
  }
  invisible(d)
}

#' Distribution of fit indices across models
plot_fit_distribution <- function(fit_table, labels = NULL) {
  d <- fit_table[fit_table$status %in% c("ok", "warning", "inadmissible") & !fit_table$saturated, , drop = FALSE]
  if (nrow(d) == 0) stop("No non-saturated models with fit indices", call. = FALSE)
  old <- graphics::par(mfrow = c(2, 2), mar = c(4, 4, 2.5, 1))
  on.exit(graphics::par(old), add = TRUE)
  # first value = "acceptable" (orange), second = "good" (green) reference line
  spec <- list(cfi = c(.90, .95), tli = c(.90, .95), rmsea = c(.08, .06), srmr = c(.10, .08))
  for (ix in names(spec)) {
    v <- d[[ix]]
    v <- v[!is.na(v)]
    if (length(v) == 0) { graphics::plot.new(); graphics::title(toupper(ix)); next }
    graphics::hist(v, breaks = min(20, max(5, length(v))), col = "#cfe0ef", border = "white",
                   main = toupper(ix), xlab = toupper(ix), ylab = labels$freq %||% "Models")
    graphics::abline(v = spec[[ix]], lty = 2, col = c("#e67e22", "#27ae60"))
  }
  invisible(NULL)
}
