# Data import, cleaning and screening.

SUPPORTED_DATA_EXT <- c("csv", "tsv", "txt", "xlsx", "xls", "sav", "dta")

#' Detect the text encoding of a file: UTF-8 (with/without BOM) or GB18030.
#' Excel on Chinese Windows saves CSV as GBK/GB18030 by default.
detect_text_encoding <- function(path) {
  raw <- readBin(path, "raw", n = min(file.info(path)$size, 2e6))
  if (length(raw) >= 3 && identical(raw[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) {
    return("UTF-8-BOM")
  }
  txt <- rawToChar(raw[raw != as.raw(0)])
  if (validUTF8(txt)) "UTF-8" else "GB18030"
}

#' Make variable names usable by lavaan (syntactic, unique)
clean_variable_names <- function(x) {
  x <- trimws(strip_bom(x))
  x[!nzchar(x)] <- "V"
  new <- make.names(x, unique = TRUE)
  new <- gsub("\\.+$", "", new)
  new[!nzchar(new)] <- "V"
  make.unique(new, sep = "_")
}

#' Read a data file
#'
#' @param path file path
#' @param name original file name (used for the extension)
#' @param encoding "auto", "UTF-8" or "GB18030" (text files only)
#' @param sheet sheet index for Excel files
#' @return list(data, info) where info has filename, encoding and a
#'   data.frame `renamed` of variables whose names had to be changed
read_data_file <- function(path, name = basename(path), encoding = "auto", sheet = 1) {
  ext <- tolower(tools::file_ext(name))
  if (!ext %in% SUPPORTED_DATA_EXT) {
    stop(sprintf("Unsupported file type: .%s", ext), call. = FALSE)
  }
  used_encoding <- NA_character_

  data <- switch(ext,
    csv = , tsv = , txt = {
      enc <- if (identical(encoding, "auto")) detect_text_encoding(path) else encoding
      used_encoding <- enc
      first <- readLines(path, n = 1, warn = FALSE)
      has <- function(ch) grepl(ch, first, fixed = TRUE, useBytes = TRUE)
      sep <- if (ext == "tsv" || (ext == "txt" && has("\t"))) "\t" else
        if (ext == "csv" && !has(",") && has(";")) ";" else ","
      utils::read.table(path, header = TRUE, sep = sep, quote = "\"",
                        fileEncoding = enc, check.names = FALSE,
                        stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A", "."),
                        comment.char = "", strip.white = TRUE)
    },
    xlsx = , xls = {
      if (requireNamespace("readxl", quietly = TRUE)) {
        as.data.frame(readxl::read_excel(path, sheet = sheet), check.names = FALSE)
      } else if (ext == "xlsx" && requireNamespace("openxlsx", quietly = TRUE)) {
        openxlsx::read.xlsx(path, sheet = sheet, check.names = FALSE)
      } else stop("Package 'readxl' is required to read Excel files", call. = FALSE)
    },
    sav = , dta = {
      if (requireNamespace("haven", quietly = TRUE)) {
        d <- if (ext == "sav") haven::read_sav(path) else haven::read_dta(path)
        as.data.frame(haven::zap_labels(haven::zap_missing(d)), check.names = FALSE)
      } else if (requireNamespace("foreign", quietly = TRUE)) {
        if (ext == "sav") {
          suppressWarnings(foreign::read.spss(path, to.data.frame = TRUE,
                                              use.value.labels = FALSE))
        } else foreign::read.dta(path, convert.factors = FALSE)
      } else stop("Package 'haven' or 'foreign' is required", call. = FALSE)
    }
  )

  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(data) == 0 || nrow(data) == 0) stop("The file contains no data", call. = FALSE)

  old <- names(data)
  new <- clean_variable_names(old)
  names(data) <- new
  renamed <- data.frame(original = old, new = new, stringsAsFactors = FALSE)
  renamed <- renamed[renamed$original != renamed$new, , drop = FALSE]

  # Convert columns that are numbers stored as text / logicals
  for (v in names(data)) {
    x <- data[[v]]
    if (is.logical(x)) data[[v]] <- as.integer(x)
    else if (is.character(x)) {
      conv <- suppressWarnings(as.numeric(x))
      if (sum(is.na(conv)) == sum(is.na(x) | !nzchar(trimws(x)))) data[[v]] <- conv
    } else if (inherits(x, c("Date", "POSIXt"))) {
      data[[v]] <- as.character(x)
    }
  }

  list(
    data = data,
    info = list(filename = name, encoding = used_encoding,
                nrow = nrow(data), ncol = ncol(data), renamed = renamed)
  )
}

#' Parse a user-entered list of missing-value codes, e.g. "-999, 99"
parse_missing_codes <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(numeric(0))
  parts <- trimws(unlist(strsplit(text, "[,;，\\s]+", perl = TRUE)))
  parts <- parts[nzchar(parts)]
  vals <- suppressWarnings(as.numeric(parts))
  if (any(is.na(vals))) {
    stop("Missing-value codes must be numbers: ",
         paste(parts[is.na(vals)], collapse = ", "), call. = FALSE)
  }
  unique(vals)
}

#' Replace user-defined missing codes in numeric columns by NA
#' @return list(data, counts) where counts is the number replaced per variable
recode_missing <- function(data, codes) {
  counts <- stats::setNames(integer(ncol(data)), names(data))
  if (length(codes) == 0) return(list(data = data, counts = counts[counts > 0]))
  for (v in names(data)) {
    x <- data[[v]]
    if (is.numeric(x)) {
      hit <- !is.na(x) & x %in% codes
      counts[v] <- sum(hit)
      x[hit] <- NA
      data[[v]] <- x
    }
  }
  list(data = data, counts = counts[counts > 0])
}

# Sample skewness and excess kurtosis with the small-sample adjustment used
# by SPSS and Stata ("type 2" in e1071/psych terminology).
skewness_g1 <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m2 <- mean((x - mean(x))^2)
  if (m2 == 0) return(NA_real_)
  g1 <- mean((x - mean(x))^3) / m2^1.5
  g1 * sqrt(n * (n - 1)) / (n - 2)
}

kurtosis_g2 <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 4) return(NA_real_)
  m2 <- mean((x - mean(x))^2)
  if (m2 == 0) return(NA_real_)
  g2 <- mean((x - mean(x))^4) / m2^2 - 3
  ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3))
}

#' Descriptive statistics for every variable
#'
#' Flags: |skewness| > 2 or excess kurtosis > 7 (West, Finch & Curran, 1995)
#' indicate substantial non-normality; > 20% missing is also flagged.
describe_data <- function(data) {
  rows <- lapply(names(data), function(v) {
    x <- data[[v]]
    n_miss <- sum(is.na(x))
    base <- data.frame(variable = v, type = class(x)[1], n = sum(!is.na(x)),
                       missing = n_miss, missing_pct = 100 * n_miss / length(x),
                       mean = NA_real_, sd = NA_real_, min = NA_real_, max = NA_real_,
                       skew = NA_real_, kurtosis = NA_real_, n_unique = length(unique(x[!is.na(x)])),
                       stringsAsFactors = FALSE)
    if (is.numeric(x) && sum(!is.na(x)) > 0) {
      base$mean <- mean(x, na.rm = TRUE)
      base$sd <- if (sum(!is.na(x)) > 1) stats::sd(x, na.rm = TRUE) else NA_real_
      base$min <- min(x, na.rm = TRUE)
      base$max <- max(x, na.rm = TRUE)
      base$skew <- skewness_g1(x)
      base$kurtosis <- kurtosis_g2(x)
    }
    base
  })
  out <- do.call(rbind, rows)
  out$flag_nonnormal <- !is.na(out$skew) & (abs(out$skew) > 2 | out$kurtosis > 7)
  out$flag_missing <- out$missing_pct > 20
  out$flag_constant <- out$n_unique <= 1
  out
}

#' One-line overview of the data set
data_overview <- function(data) {
  cc <- stats::complete.cases(data)
  list(
    n = nrow(data),
    p = ncol(data),
    n_numeric = sum(vapply(data, is.numeric, logical(1))),
    n_complete = sum(cc),
    pct_complete = 100 * mean(cc),
    pct_missing_cells = 100 * mean(is.na(data))
  )
}

#' Add mean-centred copies of variables (prefix "c_")
add_centered <- function(data, vars) {
  for (v in vars) {
    if (!is.numeric(data[[v]])) stop(sprintf("'%s' is not numeric", v), call. = FALSE)
    data[[paste0("c_", v)]] <- data[[v]] - mean(data[[v]], na.rm = TRUE)
  }
  data
}

#' Add a product term a*b (optionally of the mean-centred variables)
add_product <- function(data, a, b, center = TRUE) {
  for (v in c(a, b)) {
    if (!is.numeric(data[[v]])) stop(sprintf("'%s' is not numeric", v), call. = FALSE)
  }
  xa <- data[[a]]
  xb <- data[[b]]
  if (center) {
    xa <- xa - mean(xa, na.rm = TRUE)
    xb <- xb - mean(xb, na.rm = TRUE)
  }
  nm <- if (center) paste0("c", a, "_x_c", b) else paste0(a, "_x_", b)
  data[[make.names(nm)]] <- xa * xb
  data
}

#' Add a row-mean composite score (requires at least `min_items` answered)
add_mean_score <- function(data, name, items, min_items = length(items)) {
  for (v in items) {
    if (!is.numeric(data[[v]])) stop(sprintf("'%s' is not numeric", v), call. = FALSE)
  }
  m <- as.matrix(data[items])
  answered <- rowSums(!is.na(m))
  score <- rowMeans(m, na.rm = TRUE)
  score[answered < min_items] <- NA
  data[[make.names(name)]] <- score
  data
}
