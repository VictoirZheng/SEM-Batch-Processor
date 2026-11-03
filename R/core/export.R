# Export of batch results: Excel workbook, ZIP bundle, reproduction script.

#' Methods / settings description written to exports
#'
#' @param batch list with engine, opts, notes, started, finished, n_models,
#'   p_adjust, family, focal_types, versions, data_info, missing_codes
methods_table <- function(batch, lang = "en") {
  zh <- identical(lang, "zh")
  o <- batch$opts %||% list()
  items <- list(
    c(if (zh) "生成工具" else "Software", sprintf("SEM Batch Processor %s", batch$app_version %||% "")),
    c(if (zh) "分析引擎" else "Engine", batch$engine_label %||% batch$engine),
    c(if (zh) "运行时间" else "Run at", format(batch$started %||% Sys.time(), "%Y-%m-%d %H:%M:%S")),
    c(if (zh) "数据文件" else "Data file", batch$data_info$filename %||% ""),
    c(if (zh) "样本量（数据行数）" else "Rows in data", as.character(batch$data_info$nrow %||% "")),
    c(if (zh) "自定义缺失值编码" else "Missing-value codes", paste(batch$missing_codes %||% character(0), collapse = ", ")),
    c(if (zh) "模型数量" else "Number of models", as.character(batch$n_models %||% "")),
    c(if (zh) "组合规则" else "Combination rule", batch$combination_rule %||% "")
  )
  if (identical(batch$engine, "lavaan")) {
    items <- c(items, list(
      c(if (zh) "估计方法" else "Estimator", o$estimator %||% ""),
      c(if (zh) "缺失数据处理" else "Missing data", o$missing %||% ""),
      c(if (zh) "潜变量标识" else "Latent identification",
        if (isTRUE(o$std_lv)) (if (zh) "因子方差固定为 1 (std.lv)" else "unit variance (std.lv)") else (if (zh) "首个指标载荷固定为 1" else "marker variable")),
      c(if (zh) "Bootstrap 次数" else "Bootstrap draws", as.character(o$bootstrap %||% 0)),
      c(if (zh) "Bootstrap 置信区间" else "Bootstrap CI", if ((o$bootstrap %||% 0) > 0) o$boot_ci %||% "" else "-"),
      c(if (zh) "随机种子" else "Random seed", as.character(o$seed %||% "")),
      c(if (zh) "有序分类变量" else "Ordered variables", paste(o$ordered %||% character(0), collapse = ", ")),
      c(if (zh) "置信水平" else "Confidence level", as.character(o$level %||% 0.95))
    ))
  } else {
    items <- c(items, list(
      c(if (zh) "估计设置" else "Estimation settings",
        if (zh) "由每个模型 inp 文件中的 ANALYSIS 命令决定" else "Given by the ANALYSIS command of each .inp file")
    ))
  }
  items <- c(items, list(
    c(if (zh) "焦点效应类型" else "Focal effects", paste(batch$focal_types %||% character(0), collapse = ", ")),
    c(if (zh) "多重比较校正" else "Multiple-comparison correction", batch$p_adjust %||% ""),
    c(if (zh) "校正族" else "Correction family", batch$family %||% ""),
    c(if (zh) "软件版本" else "Versions", batch$versions %||% "")
  ))
  if (length(batch$notes) > 0) {
    items <- c(items, lapply(unique(batch$notes), function(n) c(if (zh) "自动调整" else "Automatic adjustment", n)))
  }
  out <- as.data.frame(do.call(rbind, items), stringsAsFactors = FALSE)
  names(out) <- if (zh) c("项目", "内容") else c("Item", "Value")
  out
}

#' Package versions string
version_string <- function(engine = "lavaan") {
  pk <- c("lavaan", if (identical(engine, "mplus")) "MplusAutomation")
  v <- vapply(pk, function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) "not installed"), character(1))
  paste(c(sprintf("R %s", getRversion()), sprintf("%s %s", pk, v)), collapse = "; ")
}

round_df <- function(df, digits = 4) {
  if (is.null(df)) return(df)
  for (v in names(df)) if (is.numeric(df[[v]])) df[[v]] <- round(df[[v]], digits)
  df
}

#' Write the results workbook
#'
#' @param path output .xlsx
#' @param sheets named list of data.frames (sheet name -> table)
write_results_xlsx <- function(path, sheets) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Package 'openxlsx' is required", call. = FALSE)
  wb <- openxlsx::createWorkbook()
  head_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DCE6F1",
                                      border = "Bottom", wrapText = TRUE)
  for (nm in names(sheets)) {
    df <- sheets[[nm]]
    if (is.null(df) || NROW(df) == 0) df <- data.frame(info = "-")
    sheet <- substr(gsub("[\\[\\]\\*\\?/\\\\:]", "_", nm), 1, 31)
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, round_df(df), headerStyle = head_style)
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
    openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(df)), widths = "auto")
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

#' Stand-alone R script that refits the lavaan batch
reproduce_script <- function(opts, data_file = "data.csv") {
  r <- resolve_lavaan_options(opts)
  args <- r$args
  arg_txt <- paste(vapply(names(args), function(n) {
    v <- args[[n]]
    val <- if (is.character(v)) {
      if (length(v) > 1) sprintf("c(%s)", paste(sprintf('"%s"', v), collapse = ", ")) else sprintf('"%s"', v)
    } else as.character(v)
    sprintf("%s = %s", n, val)
  }, character(1)), collapse = ",\n             ")
  c("# Reproduce the batch analysis exported by SEM Batch Processor",
    "# Requires: install.packages(\"lavaan\")",
    "library(lavaan)",
    sprintf("dat <- read.csv(\"%s\", fileEncoding = \"UTF-8\")", data_file),
    "files <- sort(list.files(\"models\", pattern = \"\\\\.lav$\", full.names = TRUE))",
    "fits <- lapply(files, function(f) {",
    "  model <- paste(readLines(f, encoding = \"UTF-8\"), collapse = \"\\n\")",
    sprintf("  set.seed(%s)", opts$seed %||% 12345),
    sprintf("  sem(model, data = dat,\n             %s)", arg_txt),
    "})",
    "names(fits) <- tools::file_path_sans_ext(basename(files))",
    sprintf("lapply(fits, parameterEstimates, boot.ci.type = \"%s\")", opts$boot_ci %||% "perc"))
}

#' Create the ZIP bundle
#'
#' @param zip_path output path
#' @param manifest manifest (id, name, syntax, placeholders)
#' @param engine "lavaan" or "mplus"
#' @param data processed data used for the analysis
#' @param xlsx_path path to an already written results workbook
#' @param results list of model results (for raw outputs)
#' @param run_dir Mplus run directory (inp/out/dat files)
#' @param opts lavaan options (for reproduce.R)
write_results_zip <- function(zip_path, manifest, engine, data, xlsx_path = NULL,
                              results = list(), run_dir = NULL, opts = list()) {
  tmp <- tempfile("bundle")
  dir.create(file.path(tmp, "models"), recursive = TRUE)
  if (!is.null(xlsx_path) && file.exists(xlsx_path)) file.copy(xlsx_path, file.path(tmp, "results.xlsx"))
  utils::write.csv(manifest[, setdiff(names(manifest), "syntax"), drop = FALSE],
                   file.path(tmp, "manifest.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (identical(engine, "lavaan")) {
    for (i in seq_len(nrow(manifest))) {
      writeLines(manifest$syntax[i], file.path(tmp, "models", paste0(manifest$id[i], ".lav")), useBytes = TRUE)
      r <- results[[manifest$id[i]]]
      if (!is.null(r) && length(r$output_text) > 0) {
        writeLines(r$output_text, file.path(tmp, "models", paste0(manifest$id[i], "_output.txt")), useBytes = TRUE)
      }
    }
    utils::write.csv(data, file.path(tmp, "data.csv"), row.names = FALSE, fileEncoding = "UTF-8")
    writeLines(reproduce_script(opts), file.path(tmp, "reproduce.R"))
  } else if (!is.null(run_dir) && dir.exists(run_dir)) {
    files <- list.files(run_dir, pattern = "\\.(inp|out|dat)$", full.names = TRUE, ignore.case = TRUE)
    file.copy(files, file.path(tmp, "models"))
    nm <- file.path(run_dir, "variable_names.csv")
    if (file.exists(nm)) file.copy(nm, tmp)
  }
  writeLines(utils::capture.output(utils::sessionInfo()), file.path(tmp, "session_info.txt"))
  old <- setwd(tmp)
  on.exit(setwd(old), add = TRUE)
  files <- list.files(".", recursive = TRUE)
  zip::zip(zip_path, files, mode = "mirror")
  invisible(zip_path)
}
