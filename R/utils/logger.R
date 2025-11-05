# Logging Utility Module
# Provides structured logging functionality for the application

#' Log levels
LOG_LEVELS <- list(
  DEBUG = 1,
  INFO = 2,
  WARNING = 3,
  ERROR = 4,
  CRITICAL = 5
)

#' Initialize logger
#' @param log_file Path to log file (character) or NULL to disable file logging
#' @param level Minimum log level
#' @param console Whether to also log to console
initialize_logger <- function(log_file = NULL, level = "INFO", console = TRUE) {
  # Validate log_file parameter
  if (!is.null(log_file) && !is.character(log_file)) {
    warning("log_file must be a character string or NULL. Disabling file logging.")
    log_file <- NULL
  }
  
  # Validate level parameter
  if (!level %in% names(LOG_LEVELS)) {
    warning(sprintf("Invalid log level '%s'. Using 'INFO'.", level))
    level <- "INFO"
  }
  
  if (!exists(".logger_config", envir = .GlobalEnv)) {
    .GlobalEnv$.logger_config <- list(
      file = log_file,
      level = LOG_LEVELS[[level]],
      console = console,
      initialized = TRUE
    )
    
    # Create log directory if file logging is enabled
    if (!is.null(log_file) && is.character(log_file)) {
      log_dir <- dirname(log_file)
      if (!dir.exists(log_dir)) {
        tryCatch({
          dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
        }, error = function(e) {
          warning(sprintf("Failed to create log directory: %s", e$message))
          .GlobalEnv$.logger_config$file <- NULL
        })
      }
    }
  }
}

#' Format log message
#' @param level Log level
#' @param message Log message
#' @param context Additional context
#' @return Formatted log string
format_log_message <- function(level, message, context = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  log_str <- sprintf("[%s] [%s] %s", timestamp, level, message)
  
  if (!is.null(context) && length(context) > 0) {
    context_str <- paste(names(context), context, sep = "=", collapse = ", ")
    log_str <- paste0(log_str, " | ", context_str)
  }
  
  return(log_str)
}

#' Write log message
#' @param level Log level
#' @param message Log message
#' @param context Additional context
write_log <- function(level, message, context = NULL) {
  if (!exists(".logger_config", envir = .GlobalEnv)) {
    initialize_logger()
  }
  
  config <- .GlobalEnv$.logger_config
  level_num <- LOG_LEVELS[[level]]
  
  if (level_num < config$level) {
    return(invisible(NULL))
  }
  
  log_message <- format_log_message(level, message, context)
  
  # Write to console
  if (config$console) {
    if (level %in% c("ERROR", "CRITICAL")) {
      message(crayon::red(log_message))
    } else if (level == "WARNING") {
      message(crayon::yellow(log_message))
    } else if (level == "DEBUG") {
      message(crayon::silver(log_message))
    } else {
      message(log_message)
    }
  }
  
  # Write to file
  if (!is.null(config$file)) {
    tryCatch({
      cat(log_message, "\n", file = config$file, append = TRUE)
    }, error = function(e) {
      warning(sprintf("Failed to write to log file: %s", e$message))
    })
  }
  
  invisible(log_message)
}

#' Log debug message
#' @param message Log message
#' @param ... Additional context as named arguments
log_debug <- function(message, ...) {
  context <- list(...)
  write_log("DEBUG", message, context)
}

#' Log info message
#' @param message Log message
#' @param ... Additional context as named arguments
log_info <- function(message, ...) {
  context <- list(...)
  write_log("INFO", message, context)
}

#' Log warning message
#' @param message Log message
#' @param ... Additional context as named arguments
log_warning <- function(message, ...) {
  context <- list(...)
  write_log("WARNING", message, context)
}

#' Log error message
#' @param message Log message
#' @param ... Additional context as named arguments
log_error <- function(message, ...) {
  context <- list(...)
  write_log("ERROR", message, context)
}

#' Log critical message
#' @param message Log message
#' @param ... Additional context as named arguments
log_critical <- function(message, ...) {
  context <- list(...)
  write_log("CRITICAL", message, context)
}

#' Log function execution
#' @param func Function to execute
#' @param func_name Function name for logging
#' @param ... Arguments to pass to function
#' @return Function result
log_execution <- function(func, func_name = deparse(substitute(func)), ...) {
  log_info(sprintf("Starting execution: %s", func_name))
  start_time <- Sys.time()
  
  result <- tryCatch({
    func(...)
  }, error = function(e) {
    log_error(sprintf("Error in %s: %s", func_name, e$message))
    stop(e)
  })
  
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  log_info(sprintf("Completed execution: %s", func_name), 
           elapsed_seconds = round(elapsed, 2))
  
  return(result)
}

#' Rotate log files
#' @param max_size_mb Maximum file size in MB before rotation
#' @param max_files Maximum number of log files to keep
rotate_log_files <- function(max_size_mb = 10, max_files = 5) {
  if (!exists(".logger_config", envir = .GlobalEnv)) {
    return(invisible(NULL))
  }
  
  config <- .GlobalEnv$.logger_config
  if (is.null(config$file)) {
    return(invisible(NULL))
  }
  
  log_file <- config$file
  
  # Check if rotation is needed
  if (file.exists(log_file)) {
    file_size_mb <- file.info(log_file)$size / (1024^2)
    
    if (file_size_mb > max_size_mb) {
      # Rotate files
      for (i in (max_files - 1):1) {
        old_file <- if (i == 1) log_file else paste0(log_file, ".", i - 1)
        new_file <- paste0(log_file, ".", i)
        
        if (file.exists(old_file)) {
          file.rename(old_file, new_file)
        }
      }
      
      # Create new log file
      file.create(log_file)
      log_info("Log file rotated")
    }
  }
  
  # Clean old log files
  log_dir <- dirname(log_file)
  log_pattern <- paste0(basename(log_file), "\\.\\d+$")
  old_logs <- list.files(log_dir, pattern = log_pattern, full.names = TRUE)
  
  if (length(old_logs) > max_files) {
    # Sort by modification time and remove oldest
    old_logs_sorted <- old_logs[order(file.info(old_logs)$mtime)]
    to_remove <- old_logs_sorted[1:(length(old_logs) - max_files)]
    file.remove(to_remove)
  }
  
  invisible(TRUE)
}

#' Create log context manager
#' @param context_name Context name
#' @return Context manager function
create_log_context <- function(context_name) {
  context_id <- generate_id("ctx")
  
  return(list(
    enter = function() {
      log_info(sprintf("Entering context: %s", context_name), context_id = context_id)
      return(context_id)
    },
    exit = function(error = NULL) {
      if (!is.null(error)) {
        log_error(sprintf("Context %s failed: %s", context_name, error$message), 
                  context_id = context_id)
      } else {
        log_info(sprintf("Exiting context: %s", context_name), context_id = context_id)
      }
    }
  ))
}

#' Log performance metrics
#' @param operation_name Operation name
#' @param metrics List of metrics
log_performance <- function(operation_name, metrics) {
  metric_str <- paste(names(metrics), metrics, sep = "=", collapse = ", ")
  log_info(sprintf("Performance: %s", operation_name), metrics = metric_str)
}

#' Create structured log entry
#' @param level Log level
#' @param message Message
#' @param ... Additional structured fields
#' @return Structured log entry
create_log_entry <- function(level, message, ...) {
  entry <- list(
    timestamp = Sys.time(),
    level = level,
    message = message,
    fields = list(...)
  )
  
  if (exists(".logger_config", envir = .GlobalEnv)) {
    config <- .GlobalEnv$.logger_config
    if (!is.null(config$file)) {
      # Append to structured log file (JSON format)
      json_file <- gsub("\\.log$", "_structured.json", config$file)
      
      tryCatch({
        if (requireNamespace("jsonlite", quietly = TRUE)) {
          json_entry <- jsonlite::toJSON(entry, auto_unbox = TRUE)
          cat(json_entry, "\n", file = json_file, append = TRUE)
        }
      }, error = function(e) {
        # Silently ignore if JSON logging fails
      })
    }
  }
  
  return(entry)
}

#' Get log statistics
#' @param log_file Path to log file
#' @return List of log statistics
get_log_statistics <- function(log_file = NULL) {
  if (is.null(log_file)) {
    if (!exists(".logger_config", envir = .GlobalEnv)) {
      return(NULL)
    }
    log_file <- .GlobalEnv$.logger_config$file
  }
  
  if (is.null(log_file) || !file.exists(log_file)) {
    return(NULL)
  }
  
  lines <- readLines(log_file, warn = FALSE)
  
  stats <- list(
    total_entries = length(lines),
    debug_count = sum(grepl("\\[DEBUG\\]", lines)),
    info_count = sum(grepl("\\[INFO\\]", lines)),
    warning_count = sum(grepl("\\[WARNING\\]", lines)),
    error_count = sum(grepl("\\[ERROR\\]", lines)),
    critical_count = sum(grepl("\\[CRITICAL\\]", lines)),
    file_size = get_file_size(log_file),
    last_modified = file.info(log_file)$mtime
  )
  
  return(stats)
}

#' Search log entries
#' @param pattern Search pattern (regex)
#' @param log_file Path to log file
#' @param max_results Maximum number of results
#' @return Matching log entries
search_logs <- function(pattern, log_file = NULL, max_results = 100) {
  if (is.null(log_file)) {
    if (!exists(".logger_config", envir = .GlobalEnv)) {
      return(character(0))
    }
    log_file <- .GlobalEnv$.logger_config$file
  }
  
  if (is.null(log_file) || !file.exists(log_file)) {
    return(character(0))
  }
  
  lines <- readLines(log_file, warn = FALSE)
  matches <- grep(pattern, lines, value = TRUE, ignore.case = TRUE)
  
  if (length(matches) > max_results) {
    matches <- matches[1:max_results]
  }
  
  return(matches)
}
