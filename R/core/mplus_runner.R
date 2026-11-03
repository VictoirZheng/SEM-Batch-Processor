# Mplus engine: variable-name mapping, .dat/.inp generation, execution and
# output parsing. Input generation does not require MplusAutomation; parsing
# the output does.

MPLUS_MAX_LINE <- 90L
MPLUS_WRAP_AT <- 85L
MPLUS_SECTION_REGEX <- "^\\s*(TITLE|DATA|VARIABLE|DEFINE|ANALYSIS|MODEL(\\s+(INDIRECT|CONSTRAINT|TEST|PRIORS|POPULATION|MISSING|COVERAGE))?|OUTPUT|SAVEDATA|PLOT|MONTECARLO)\\s*:"

# ---------------------------------------------------------------------------
# Executable detection
# ---------------------------------------------------------------------------

#' Locate the Mplus executable
#' @param configured path set by the user/config (checked first)
#' @return path or "" when not found
detect_mplus <- function(configured = NULL) {
  cand <- c(configured %||% character(0))
  cand <- c(cand, Sys.which(c("mplus", "Mplus", "Mplus.exe", "mpdemo")))
  if (.Platform$OS.type == "windows") {
    pf <- c(Sys.getenv("ProgramFiles"), Sys.getenv("ProgramFiles(x86)"),
            "C:/Program Files", "C:/Program Files (x86)")
    cand <- c(cand, file.path(pf, "Mplus", "Mplus.exe"),
              file.path(pf, "Mplus Demo", "Mpdemo.exe"))
  } else {
    cand <- c(cand, "/Applications/Mplus/mplus", "/Applications/MplusDemo/mpdemo",
              "/opt/mplus/mplus", Sys.glob("/opt/mplus/*/mplus"), "/usr/local/bin/mplus")
  }
  cand <- unique(cand[nzchar(cand)])
  hit <- cand[file.exists(cand) & !dir.exists(cand)]
  if (length(hit) == 0) "" else normalizePath(hit[1], winslash = "/")
}

# ---------------------------------------------------------------------------
# Variable names
# ---------------------------------------------------------------------------

#' Map data variable names to valid Mplus names
#'
#' Mplus names: <= 8 characters, start with a letter, letters/digits/_,
#' case-insensitive. Valid names are kept; others get a short unique name.
#' @return data.frame(original, mplus, changed)
mplus_name_map <- function(names_) {
  valid <- grepl("^[A-Za-z][A-Za-z0-9_]{0,7}$", names_)
  out <- character(length(names_))
  used <- character(0)
  # keep valid names first (respecting case-insensitive uniqueness)
  for (i in which(valid)) {
    if (!toupper(names_[i]) %in% used) {
      out[i] <- names_[i]
      used <- c(used, toupper(names_[i]))
    } else valid[i] <- FALSE
  }
  for (i in which(!valid)) {
    base <- gsub("[^A-Za-z0-9_]", "_", names_[i])
    if (!grepl("[A-Za-z0-9]", base)) base <- paste0("V", i)   # e.g. Chinese names
    if (!grepl("^[A-Za-z]", base)) base <- paste0("V", base)
    cand <- substr(base, 1, 8)
    k <- 1
    while (toupper(cand) %in% used) {
      suffix <- as.character(k)
      cand <- paste0(substr(base, 1, 8 - nchar(suffix)), suffix)
      k <- k + 1
    }
    out[i] <- cand
    used <- c(used, toupper(cand))
  }
  data.frame(original = names_, mplus = out, changed = names_ != out,
             stringsAsFactors = FALSE)
}

#' Replace original variable names by their Mplus names in syntax
rename_vars_in_syntax <- function(text, name_map) {
  nm <- name_map[name_map$changed, , drop = FALSE]
  if (nrow(nm) == 0) return(text)
  nm <- nm[order(-nchar(nm$original)), , drop = FALSE]
  for (i in seq_len(nrow(nm))) {
    pattern <- paste0("(?<![A-Za-z0-9_.])", regex_escape(nm$original[i]), "(?![A-Za-z0-9_.])")
    text <- gsub(pattern, nm$mplus[i], text, perl = TRUE)
  }
  text
}

#' Map Mplus (upper-case) names in output back to the original names
unmap_mplus_names <- function(x, name_map) {
  if (is.null(name_map) || length(x) == 0) return(x)
  lookup <- stats::setNames(name_map$original, toupper(name_map$mplus))
  hit <- toupper(x) %in% names(lookup)
  x[hit] <- unname(lookup[toupper(x[hit])])
  x
}

# ---------------------------------------------------------------------------
# Data file
# ---------------------------------------------------------------------------

#' Write a free-format Mplus data file; missing values are written as "."
write_mplus_data <- function(data, path) {
  non_num <- names(data)[!vapply(data, function(x) is.numeric(x) || is.logical(x), logical(1))]
  if (length(non_num) > 0) {
    stop("Mplus needs numeric data. Non-numeric variables: ",
         paste(non_num, collapse = ", "),
         ". Recode them or remove them from the data.", call. = FALSE)
  }
  for (v in names(data)) if (is.logical(data[[v]])) data[[v]] <- as.integer(data[[v]])
  old <- options(scipen = 100)
  on.exit(options(old))
  utils::write.table(data, path, sep = " ", na = ".", quote = FALSE,
                     row.names = FALSE, col.names = FALSE)
  invisible(path)
}

# ---------------------------------------------------------------------------
# Input file
# ---------------------------------------------------------------------------

# Split an Mplus input into its command sections
split_mplus_sections <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  code_part <- sub("!.*$", "", lines)
  is_head <- grepl(MPLUS_SECTION_REGEX, code_part, ignore.case = TRUE, perl = TRUE)
  if (!any(is_head)) {
    return(list(list(name = "", lines = lines)))
  }
  starts <- which(is_head)
  secs <- list()
  if (starts[1] > 1) secs[[1]] <- list(name = "", lines = lines[seq_len(starts[1] - 1)])
  for (k in seq_along(starts)) {
    from <- starts[k]
    to <- if (k < length(starts)) starts[k + 1] - 1 else length(lines)
    head <- regmatches(code_part[from], regexpr(MPLUS_SECTION_REGEX, code_part[from],
                                                ignore.case = TRUE, perl = TRUE))
    nm <- toupper(gsub("\\s+", " ", trimws(sub(":\\s*$", "", head))))
    secs[[length(secs) + 1]] <- list(name = nm, lines = lines[from:to])
  }
  secs
}

# Statements of a section body (comments removed), header stripped
section_statements <- function(sec) {
  body <- paste(sub("!.*$", "", sec$lines), collapse = "\n")
  body <- sub(MPLUS_SECTION_REGEX, "", body, ignore.case = TRUE, perl = TRUE)
  st <- trimws(strsplit(body, ";", fixed = TRUE)[[1]])
  st[nzchar(st)]
}

#' Wrap a list of words into lines of at most `width` characters
wrap_words <- function(words, first_prefix = "  ", next_prefix = "    ", width = MPLUS_WRAP_AT) {
  lines <- character(0)
  cur <- first_prefix
  for (w in words) {
    candidate <- if (identical(cur, first_prefix) || identical(cur, next_prefix)) paste0(cur, w) else paste(cur, w)
    if (nchar(candidate) > width && !(identical(cur, first_prefix) || identical(cur, next_prefix))) {
      lines <- c(lines, cur)
      cur <- paste0(next_prefix, w)
    } else {
      cur <- candidate
    }
  }
  c(lines, cur)
}

#' Break lines longer than Mplus' 90-character limit
#' @return list(text, notes)
wrap_mplus_lines <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  out <- character(0)
  too_long <- 0L
  for (ln in lines) {
    if (nchar(ln) <= MPLUS_MAX_LINE) {
      out <- c(out, ln)
      next
    }
    pos <- regexpr("!", ln, fixed = TRUE)
    code <- if (pos > 0) substr(ln, 1, pos - 1) else ln
    comment <- if (pos > 0) substr(ln, pos, nchar(ln)) else ""
    indent <- sub("^(\\s*).*$", "\\1", code)
    words <- strsplit(trimws(code), "\\s+")[[1]]
    words <- words[nzchar(words)]
    wrapped <- if (length(words) > 0) wrap_words(words, indent, paste0(indent, "  ")) else character(0)
    if (any(nchar(wrapped) > MPLUS_MAX_LINE)) too_long <- too_long + 1L
    if (nzchar(comment)) {
      cwords <- strsplit(sub("^!\\s*", "", comment), "\\s+")[[1]]
      wrapped <- c(wrapped, wrap_words(cwords, paste0(indent, "! "), paste0(indent, "! ")))
    }
    out <- c(out, wrapped)
  }
  notes <- if (too_long > 0) sprintf("%d line(s) still exceed %d characters (a single token is too long).", too_long, MPLUS_MAX_LINE) else character(0)
  list(text = paste(out, collapse = "\n"), notes = notes)
}

#' Assemble a complete Mplus input file
#'
#' The program writes TITLE, DATA FILE, NAMES and MISSING itself; the
#' corresponding statements in the template are removed (with a note) so
#' that the input always matches the generated data file.
#'
#' @param syntax filled template (placeholders replaced by original names)
#' @param name_map result of mplus_name_map() for all data columns
#' @param title title text
#' @param data_file data file name relative to the input file
#' @return list(text, notes, usevariables, missing_codes) where
#'   missing_codes are numeric codes declared in the template's MISSING
#'   statement (the caller should check that the data no longer contain them)
build_mplus_input <- function(syntax, name_map, title = "SEM model", data_file = "data.dat") {
  notes <- character(0)
  syntax <- strip_bom(syntax)
  syntax <- rename_vars_in_syntax(syntax, name_map)
  secs <- split_mplus_sections(syntax)
  sec_names <- vapply(secs, `[[`, character(1), "name")

  if (any(sec_names == "TITLE")) notes <- c(notes, "TITLE in the template was replaced by the generated title.")
  data_extra <- character(0)
  if (any(sec_names == "DATA")) {
    st <- unlist(lapply(secs[sec_names == "DATA"], section_statements))
    drop <- grepl("^FILE\\b", st, ignore.case = TRUE)
    if (any(drop)) notes <- c(notes, "DATA: FILE in the template was replaced by the generated data file.")
    data_extra <- st[!drop]
  }
  var_extra <- character(0)
  missing_codes <- numeric(0)
  if (any(sec_names == "VARIABLE")) {
    st <- unlist(lapply(secs[sec_names == "VARIABLE"], section_statements))
    is_names <- grepl("^NAMES\\b", st, ignore.case = TRUE)
    is_missing <- grepl("^MISSING\\b", st, ignore.case = TRUE)
    if (any(is_names)) notes <- c(notes, "NAMES in the template was replaced (generated from the data).")
    if (any(is_missing)) {
      notes <- c(notes, sprintf("'%s' in the template was replaced by 'MISSING = .' (missing values are written as '.' in the data file; recode numeric missing codes on the Data page).",
                                st[is_missing][1]))
    }
    var_extra <- st[!is_names & !is_missing]
    if (any(is_missing)) {
      nums <- regmatches(st[is_missing], gregexpr("-?[0-9]+(\\.[0-9]+)?", st[is_missing]))
      missing_codes <- unique(suppressWarnings(as.numeric(unlist(nums))))
    }
  }

  usev <- var_extra[grepl("^USEV", var_extra, ignore.case = TRUE)]
  usevariables <- character(0)
  if (length(usev) == 0) {
    notes <- c(notes, "No USEVARIABLES statement: Mplus will use every variable in NAMES.")
  } else {
    usevariables <- strsplit(trimws(sub("^USEV\\w*\\s*(=|ARE|IS)?", "", usev[1], ignore.case = TRUE)), "\\s+")[[1]]
    usevariables <- usevariables[nzchar(usevariables)]
    known <- toupper(name_map$mplus)
    plain <- usevariables[!grepl("[-*]", usevariables)]
    unknown <- plain[!toupper(plain) %in% known]
    # variables created in DEFINE are allowed
    defined <- character(0)
    if (any(sec_names == "DEFINE")) {
      dst <- unlist(lapply(secs[sec_names == "DEFINE"], section_statements))
      defined <- toupper(trimws(sub("=.*$", "", dst[grepl("=", dst)])))
    }
    unknown <- unknown[!toupper(unknown) %in% defined]
    if (length(unknown) > 0) {
      notes <- c(notes, paste("USEVARIABLES contains names not in the data:", paste(unknown, collapse = ", ")))
    }
  }

  title_line <- substr(gsub("[\r\n;]", " ", title), 1, MPLUS_WRAP_AT - 8)
  header <- c(
    paste("TITLE:", title_line),
    "",
    "DATA:",
    sprintf("  FILE = \"%s\";", data_file),
    if (length(data_extra) > 0) paste0("  ", data_extra, ";"),
    "",
    "VARIABLE:",
    wrap_words(c("NAMES =", paste0(name_map$mplus[-nrow(name_map)]),
                 paste0(name_map$mplus[nrow(name_map)], ";"))),
    "  MISSING = .;",
    if (length(var_extra) > 0) unlist(lapply(var_extra, function(s) {
      wrap_words(c(strsplit(s, "\\s+")[[1]][-length(strsplit(s, "\\s+")[[1]])],
                   paste0(utils::tail(strsplit(s, "\\s+")[[1]], 1), ";")))
    }))
  )

  rest <- secs[!sec_names %in% c("", "TITLE", "DATA", "VARIABLE")]
  pre <- secs[sec_names == ""]
  pre_text <- unlist(lapply(pre, `[[`, "lines"))
  if (length(pre_text) > 0 && any(nzchar(trimws(sub("!.*$", "", pre_text))))) {
    notes <- c(notes, "Text before the first Mplus command was kept as a comment.")
    pre_text <- ifelse(nzchar(trimws(pre_text)) & !grepl("^\\s*!", pre_text),
                       paste("!", pre_text), pre_text)
  }
  body <- unlist(lapply(rest, `[[`, "lines"))
  if (!any(vapply(rest, `[[`, character(1), "name") == "MODEL")) {
    notes <- c(notes, "No MODEL command found in the template.")
  }
  if (!any(vapply(rest, `[[`, character(1), "name") == "OUTPUT")) {
    body <- c(body, "", "OUTPUT: STANDARDIZED CINTERVAL;")
    notes <- c(notes, "OUTPUT: STANDARDIZED CINTERVAL; was added.")
  }

  text <- paste(c(header, "", pre_text, body), collapse = "\n")
  text <- gsub("\n{3,}", "\n\n", text)
  wrapped <- wrap_mplus_lines(text)
  list(text = wrapped$text, notes = c(notes, wrapped$notes), usevariables = usevariables,
       missing_codes = missing_codes[!is.na(missing_codes)])
}

# ---------------------------------------------------------------------------
# Running and parsing
# ---------------------------------------------------------------------------

#' Run Mplus on one input file (blocking; used inside a worker process)
#' @return exit status (integer) or an error string
run_mplus_process <- function(mplus_path, inp_file, timeout = Inf) {
  wd <- dirname(normalizePath(inp_file, winslash = "/"))
  inp <- basename(inp_file)
  out <- sub("\\.inp$", ".out", inp, ignore.case = TRUE)
  if (requireNamespace("processx", quietly = TRUE)) {
    r <- processx::run(mplus_path, c(inp, out), wd = wd, error_on_status = FALSE,
                       timeout = if (is.finite(timeout)) timeout else NULL,
                       windows_hide_window = TRUE)
    if (isTRUE(r$timeout)) return("timeout")
    return(r$status)
  }
  old <- setwd(wd)
  on.exit(setwd(old))
  system2(mplus_path, c(shQuote(inp), shQuote(out)), stdout = FALSE, stderr = FALSE)
}

MPLUS_INADMISSIBLE_PATTERNS <- c(
  "IS NOT POSITIVE DEFINITE",
  "MAY NOT BE TRUSTWORTHY",
  "NONIDENTIFICATION",
  "NEGATIVE RESIDUAL VARIANCE",
  "CORRELATION GREATER OR EQUAL TO ONE"
)

#' Parse an Mplus output file into the unified result format
parse_mplus_output <- function(out_file, id = "M001", name_map = NULL) {
  res <- new_model_result(id, "mplus")
  if (!file.exists(out_file)) {
    res$errors <- "Mplus did not produce an output file"
    return(res)
  }
  text <- readLines(out_file, warn = FALSE)
  res$output_text <- text
  up <- toupper(text)

  normal <- any(grepl("THE MODEL ESTIMATION TERMINATED NORMALLY", up, fixed = TRUE))
  err_lines <- grep("^\\s*\\*\\*\\* ERROR", text, value = TRUE)
  if (length(err_lines) > 0) {
    idx <- grep("^\\s*\\*\\*\\* ERROR", text)
    msgs <- vapply(idx, function(i) paste(trimws(text[i:min(length(text), i + 3)]), collapse = " "), character(1))
    res$errors <- unique(msgs)
  }
  if (any(grepl("NO CONVERGENCE", up, fixed = TRUE))) res$errors <- c(res$errors, "No convergence")
  if (!normal && length(res$errors) == 0) res$errors <- "Model estimation did not terminate normally"

  inad_hits <- vapply(MPLUS_INADMISSIBLE_PATTERNS, function(p) any(grepl(p, up, fixed = TRUE)), logical(1))
  warn_idx <- grep("^\\s*\\*\\*\\* WARNING|^\\s*WARNING:", text)
  warns <- vapply(warn_idx, function(i) paste(trimws(text[i:min(length(text), i + 2)]), collapse = " "), character(1))
  inad_lines <- unlist(lapply(MPLUS_INADMISSIBLE_PATTERNS[inad_hits], function(p) {
    i <- grep(p, up, fixed = TRUE)[1]
    paste(trimws(text[max(1, i - 1):min(length(text), i + 1)]), collapse = " ")
  }))
  res$warnings <- unique(c(inad_lines, warns))

  if (!requireNamespace("MplusAutomation", quietly = TRUE)) {
    res$errors <- c(res$errors, "Package 'MplusAutomation' is required to read Mplus output")
    res$status <- "failed"
    return(res)
  }
  mo <- tryCatch(suppressMessages(suppressWarnings(
    MplusAutomation::readModels(out_file, quiet = TRUE))), error = function(e) e)
  if (inherits(mo, "error")) {
    res$errors <- c(res$errors, paste("Could not read output:", conditionMessage(mo)))
    res$status <- "failed"
    return(res)
  }

  s <- mo$summaries
  if (!is.null(s) && length(s) > 0) {
    g <- function(f) num_or_na(s[[f]])
    res$fit[] <- c(n = g("Observations"), npar = g("Parameters"),
                   chisq = g("ChiSqM_Value"), df = g("ChiSqM_DF"), pvalue = g("ChiSqM_PValue"),
                   cfi = g("CFI"), tli = g("TLI"), rmsea = g("RMSEA_Estimate"),
                   rmsea_lower = g("RMSEA_90CI_LB"), rmsea_upper = g("RMSEA_90CI_UB"),
                   srmr = g("SRMR"), aic = g("AIC"), bic = g("BIC"))[FIT_FIELDS]
    est <- toupper(as.character(scalar_or(s$Estimator, "")))
    res$fit_robust <- est %in% ROBUST_ESTIMATORS
  }

  res$params <- tryCatch(mplus_params_table(mo, name_map), error = function(e) {
    res$warnings <<- c(res$warnings, paste("Parameter extraction:", conditionMessage(e)))
    empty_param_table()
  })

  inadmissible <- any(inad_hits)
  if (nrow(res$params) > 0) {
    pv <- res$params
    neg <- pv$op == "~~" & pv$lhs == pv$rhs & !is.na(pv$est) & pv$est < 0
    if (any(neg)) {
      inadmissible <- TRUE
      res$warnings <- c(res$warnings, paste("Negative variance estimate(s):", paste(unique(pv$lhs[neg]), collapse = ", ")))
    }
  }
  res$warnings <- unique(res$warnings)
  res$status <- classify_status(res$errors, res$warnings, normal, inadmissible)
  res
}

# Convert MplusAutomation parameter + indirect tables to the unified format
mplus_params_table <- function(mo, name_map = NULL) {
  P <- mo$parameters
  rows <- list()
  if (!is.null(P$unstandardized) && NROW(P$unstandardized) > 0) {
    rows[[1]] <- mplus_param_block(P$unstandardized, P$ci.unstandardized,
                                   P$stdyx.standardized, P$ci.stdyx.standardized)
  }
  ind <- mo$indirect
  if (!is.null(ind) && !is.null(ind$unstandardized)) {
    rows[[length(rows) + 1]] <- mplus_indirect_block(ind)
  }
  if (length(rows) == 0) return(empty_param_table())
  out <- do.call(rbind, rows)
  out$lhs <- unmap_mplus_names(out$lhs, name_map)
  out$rhs <- unmap_mplus_names(out$rhs, name_map)
  path_rows <- out$op %in% c("ind", "total_ind", "total", "direct")
  if (any(path_rows) && !is.null(name_map)) {
    out$label[path_rows] <- vapply(strsplit(out$label[path_rows], " -> ", fixed = TRUE), function(tok) {
      paste(unmap_mplus_names(tok, name_map), collapse = " -> ")
    }, character(1))
  }
  rownames(out) <- NULL
  out
}

mplus_key_cols <- function(df) intersect(c("Group", "BetweenWithin", "LatentClass"), names(df))

mplus_param_block <- function(u, ci = NULL, s = NULL, sci = NULL) {
  keyf <- function(df) {
    kc <- mplus_key_cols(df)
    do.call(paste, c(lapply(c("paramHeader", "param", kc), function(k) as.character(df[[k]])), sep = "\r"))
  }
  num <- function(df, col, m) if (!is.null(df) && col %in% names(df)) suppressWarnings(as.numeric(df[[col]]))[m] else rep(NA_real_, length(m))
  k <- keyf(u)
  m_ci <- if (!is.null(ci)) match(k, keyf(ci)) else rep(NA_integer_, length(k))
  m_s <- if (!is.null(s)) match(k, keyf(s)) else rep(NA_integer_, length(k))
  m_sci <- if (!is.null(sci)) match(k, keyf(sci)) else rep(NA_integer_, length(k))

  hdr <- as.character(u$paramHeader)
  prm <- as.character(u$param)
  op <- rep(NA_character_, length(hdr))
  lhs <- hdr
  rhs <- prm
  for (kw in c("ON", "BY", "WITH")) {
    hit <- grepl(paste0("\\.", kw, "$"), hdr)
    lhs[hit] <- sub(paste0("\\.", kw, "$"), "", hdr[hit])
    op[hit] <- c(ON = "~", BY = "=~", WITH = "~~")[[kw]]
  }
  hit <- hdr %in% c("Intercepts", "Means")
  op[hit] <- "~1"; lhs[hit] <- prm[hit]; rhs[hit] <- ""
  hit <- hdr %in% c("Variances", "Residual.Variances")
  op[hit] <- "~~"; lhs[hit] <- prm[hit]; rhs[hit] <- prm[hit]
  hit <- hdr == "Thresholds"
  op[hit] <- "|"; lhs[hit] <- sub("\\$.*$", "", prm[hit]); rhs[hit] <- sub("^[^$]*\\$", "t", prm[hit])
  hit <- hdr == "New.Additional.Parameters"
  op[hit] <- ":="; lhs[hit] <- prm[hit]; rhs[hit] <- ""
  other <- is.na(op)
  op[other] <- hdr[other]

  grp <- if ("Group" %in% names(u)) as.character(u$Group) else rep("", length(hdr))
  label <- ifelse(op == ":=", prm, "")
  est <- num(u, "est", seq_along(hdr))
  se <- num(u, "se", seq_along(hdr))
  z <- num(u, "est_se", seq_along(hdr))
  p <- num(u, "pval", seq_along(hdr))
  fixed <- !is.na(se) & se == 0
  z[fixed] <- NA; p[fixed] <- NA
  data.frame(
    group = grp, lhs = lhs, op = op, rhs = rhs, label = label,
    est = est, se = se, z = z, p = p,
    ci_lower = num(ci, "low2.5", m_ci), ci_upper = num(ci, "up2.5", m_ci),
    std_est = num(s, "est", m_s), std_se = num(s, "se", m_s), std_p = num(s, "pval", m_s),
    std_ci_lower = num(sci, "low2.5", m_sci), std_ci_upper = num(sci, "up2.5", m_sci),
    stringsAsFactors = FALSE
  )
}

mplus_indirect_block <- function(ind) {
  part <- function(section, kind) {
    x <- ind[[section]]
    if (is.null(x)) return(NULL)
    d <- x[[kind]]
    if (is.null(d) || NROW(d) == 0) return(NULL)
    as.data.frame(d, stringsAsFactors = FALSE)
  }
  key_spec <- function(d) paste(d$pred, d$intervening, d$outcome, if ("group" %in% names(d)) d$group else "", sep = "\r")
  key_over <- function(d) paste(d$pred, d$outcome, d$summary, if ("group" %in% names(d)) d$group else "", sep = "\r")
  num <- function(d, col, m) if (!is.null(d) && col %in% names(d)) suppressWarnings(as.numeric(d[[col]]))[m] else rep(NA_real_, length(m))

  build <- function(kind, keyfun) {
    u <- part("unstandardized", kind)
    if (is.null(u)) return(NULL)
    k <- keyfun(u)
    ci <- part("ci.unstandardized", kind)
    s <- part("stdyx.standardized", kind)
    sci <- part("ci.stdyx.standardized", kind)
    mi <- function(d) if (is.null(d)) rep(NA_integer_, length(k)) else match(k, keyfun(d))
    m_ci <- mi(ci); m_s <- mi(s); m_sci <- mi(sci)
    if (kind == "specific") {
      mids <- strsplit(as.character(u$intervening), ".", fixed = TRUE)
      label <- mapply(function(p, m, o) paste(c(p, m, o), collapse = " -> "),
                      as.character(u$pred), mids, as.character(u$outcome))
      op <- rep("ind", nrow(u))
    } else {
      sm <- tolower(as.character(u$summary))
      # MplusAutomation labels: "Total", "Total indirect", "Sum of indirect",
      # "Indirect", "Direct"
      op <- ifelse(grepl("indirect", sm), "total_ind",
                   ifelse(grepl("^total", sm), "total", ifelse(grepl("direct", sm), "direct", sm)))
      label <- paste(u$pred, "->", u$outcome)
    }
    rr <- seq_len(nrow(u))
    data.frame(
      group = if ("group" %in% names(u)) as.character(u$group) else "",
      lhs = as.character(u$outcome), op = op, rhs = as.character(u$pred),
      label = unname(label),
      est = num(u, "est", rr), se = num(u, "se", rr), z = num(u, "est_se", rr), p = num(u, "pval", rr),
      ci_lower = num(ci, "low2.5", m_ci), ci_upper = num(ci, "up2.5", m_ci),
      std_est = num(s, "est", m_s), std_se = num(s, "se", m_s), std_p = num(s, "pval", m_s),
      std_ci_lower = num(sci, "low2.5", m_sci), std_ci_upper = num(sci, "up2.5", m_sci),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, Filter(Negate(is.null), list(build("overall", key_over), build("specific", key_spec))))
}
