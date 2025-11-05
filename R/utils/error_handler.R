# Error Handling Utility Module
# Provides comprehensive error handling and recovery mechanisms

#' Error types classification
ERROR_TYPES <- list(
  DATA_ERROR = "DATA_ERROR",
  TEMPLATE_ERROR = "TEMPLATE_ERROR",
  ENGINE_ERROR = "ENGINE_ERROR",
  SYSTEM_ERROR = "SYSTEM_ERROR",
  VALIDATION_ERROR = "VALIDATION_ERROR"
)

#' Create custom error
#' @param message Error message
#' @param type Error type
#' @param details Additional error details
#' @return Error object
create_error <- function(message, type = ERROR_TYPES$SYSTEM_ERROR, details = NULL) {
  err <- list(
    message = message,
    type = type,
    details = details,
    timestamp = Sys.time(),
    call = sys.call(-1)
  )
  class(err) <- c("sem_error", "error", "condition")
  return(err)
}

#' Handle error with recovery
#' @param error Error object
#' @param recovery_func Optional recovery function
#' @return Recovery result or NULL
handle_error <- function(error, recovery_func = NULL) {
  # Log the error
  if (exists("log_error", mode = "function")) {
    log_error(
      error$message,
      type = error$type,
      details = if (!is.null(error$details)) toString(error$details) else NA
    )
  }
  
  # Attempt recovery if function provided
  if (!is.null(recovery_func) && is.function(recovery_func)) {
    tryCatch({
      result <- recovery_func(error)
      if (exists("log_info", mode = "function")) {
        log_info("Error recovery successful")
      }
      return(result)
    }, error = function(e) {
      if (exists("log_error", mode = "function")) {
        log_error(sprintf("Error recovery failed: %s", e$message))
      }
      return(NULL)
    })
  }
  
  return(NULL)
}

#' Retry operation with exponential backoff
#' @param operation Function to retry
#' @param max_attempts Maximum number of attempts
#' @param initial_delay Initial delay in seconds
#' @param max_delay Maximum delay in seconds
#' @param ... Arguments to pass to operation
#' @return Operation result
retry_with_backoff <- function(operation, max_attempts = 3, 
                               initial_delay = 1, max_delay = 60, ...) {
  attempt <- 1
  delay <- initial_delay
  
  while (attempt <= max_attempts) {
    result <- tryCatch({
      operation(...)
    }, error = function(e) {
      e
    })
    
    if (!inherits(result, "error")) {
      if (attempt > 1 && exists("log_info", mode = "function")) {
        log_info(sprintf("Operation succeeded on attempt %d", attempt))
      }
      return(result)
    }
    
    if (attempt < max_attempts) {
      if (exists("log_warning", mode = "function")) {
        log_warning(
          sprintf("Attempt %d failed, retrying in %d seconds", attempt, delay),
          error = result$message
        )
      }
      Sys.sleep(delay)
      delay <- min(delay * 2, max_delay)  # Exponential backoff
    }
    
    attempt <- attempt + 1
  }
  
  # All attempts failed
  if (exists("log_error", mode = "function")) {
    log_error(sprintf("Operation failed after %d attempts", max_attempts))
  }
  stop(result)
}

#' Safe execution wrapper
#' @param expr Expression to execute
#' @param error_handler Error handling function
#' @param finally_func Function to execute regardless of success/failure
#' @return Execution result or error
safe_execute <- function(expr, error_handler = NULL, finally_func = NULL) {
  result <- tryCatch({
    eval(expr, envir = parent.frame())
  }, error = function(e) {
    if (!is.null(error_handler)) {
      error_handler(e)
    } else {
      if (exists("log_error", mode = "function")) {
        log_error(sprintf("Execution error: %s", e$message))
      }
      e
    }
  }, finally = {
    if (!is.null(finally_func)) {
      finally_func()
    }
  })
  
  return(result)
}

#' Validate input with custom error
#' @param condition Validation condition
#' @param error_message Error message if validation fails
#' @param error_type Error type
validate_input <- function(condition, error_message, 
                          error_type = ERROR_TYPES$VALIDATION_ERROR) {
  if (!condition) {
    err <- create_error(error_message, error_type)
    stop(err)
  }
  invisible(TRUE)
}

#' Create checkpoint for recovery
#' @param state State object to save
#' @param checkpoint_id Unique checkpoint identifier
#' @return Checkpoint file path
create_checkpoint <- function(state, checkpoint_id = NULL) {
  if (is.null(checkpoint_id)) {
    checkpoint_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
  }
  
  checkpoint_dir <- file.path(tempdir(), "checkpoints")
  if (!dir.exists(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE)
  }
  
  checkpoint_file <- file.path(checkpoint_dir, paste0("checkpoint_", checkpoint_id, ".rds"))
  
  tryCatch({
    saveRDS(state, checkpoint_file)
    if (exists("log_info", mode = "function")) {
      log_info("Checkpoint created", checkpoint_id = checkpoint_id)
    }
    return(checkpoint_file)
  }, error = function(e) {
    if (exists("log_error", mode = "function")) {
      log_error(sprintf("Failed to create checkpoint: %s", e$message))
    }
    return(NULL)
  })
}

#' Restore from checkpoint
#' @param checkpoint_id Checkpoint identifier or file path
#' @return Restored state or NULL
restore_checkpoint <- function(checkpoint_id) {
  checkpoint_file <- if (file.exists(checkpoint_id)) {
    checkpoint_id
  } else {
    file.path(tempdir(), "checkpoints", paste0("checkpoint_", checkpoint_id, ".rds"))
  }
  
  if (!file.exists(checkpoint_file)) {
    if (exists("log_warning", mode = "function")) {
      log_warning("Checkpoint file not found", checkpoint_id = checkpoint_id)
    }
    return(NULL)
  }
  
  tryCatch({
    state <- readRDS(checkpoint_file)
    if (exists("log_info", mode = "function")) {
      log_info("Checkpoint restored", checkpoint_id = checkpoint_id)
    }
    return(state)
  }, error = function(e) {
    if (exists("log_error", mode = "function")) {
      log_error(sprintf("Failed to restore checkpoint: %s", e$message))
    }
    return(NULL)
  })
}

#' List available checkpoints
#' @return Vector of checkpoint IDs
list_checkpoints <- function() {
  checkpoint_dir <- file.path(tempdir(), "checkpoints")
  if (!dir.exists(checkpoint_dir)) {
    return(character(0))
  }
  
  files <- list.files(checkpoint_dir, pattern = "^checkpoint_.*\\.rds$", full.names = FALSE)
  checkpoint_ids <- gsub("^checkpoint_|\\.rds$", "", files)
  return(checkpoint_ids)
}

#' Clean old checkpoints
#' @param max_age_hours Maximum age in hours
clean_checkpoints <- function(max_age_hours = 24) {
  checkpoint_dir <- file.path(tempdir(), "checkpoints")
  if (!dir.exists(checkpoint_dir)) {
    return(invisible(0))
  }
  
  files <- list.files(checkpoint_dir, pattern = "^checkpoint_.*\\.rds$", full.names = TRUE)
  current_time <- Sys.time()
  removed_count <- 0
  
  for (file in files) {
    file_age <- difftime(current_time, file.info(file)$mtime, units = "hours")
    if (file_age > max_age_hours) {
      file.remove(file)
      removed_count <- removed_count + 1
    }
  }
  
  if (removed_count > 0 && exists("log_info", mode = "function")) {
    log_info(sprintf("Cleaned %d old checkpoints", removed_count))
  }
  
  invisible(removed_count)
}

#' Error recovery strategies
ERROR_RECOVERY_STRATEGIES <- list(
  DATA_ERROR = function(error) {
    # Try to recover from data errors
    if (exists("log_info", mode = "function")) {
      log_info("Attempting data error recovery")
    }
    return(NULL)
  },
  
  TEMPLATE_ERROR = function(error) {
    # Try to recover from template errors
    if (exists("log_info", mode = "function")) {
      log_info("Attempting template error recovery")
    }
    return(NULL)
  },
  
  ENGINE_ERROR = function(error) {
    # Try to switch to alternative engine
    if (exists("log_info", mode = "function")) {
      log_info("Attempting engine error recovery - switching engines")
    }
    return(NULL)
  },
  
  SYSTEM_ERROR = function(error) {
    # Try to recover from system errors
    if (exists("log_info", mode = "function")) {
      log_info("Attempting system error recovery")
    }
    return(NULL)
  }
)

#' Get error recovery strategy
#' @param error_type Error type
#' @return Recovery function or NULL
get_recovery_strategy <- function(error_type) {
  if (error_type %in% names(ERROR_RECOVERY_STRATEGIES)) {
    return(ERROR_RECOVERY_STRATEGIES[[error_type]])
  }
  return(NULL)
}

#' Create error report
#' @param error Error object
#' @param include_traceback Whether to include traceback
#' @return Error report as list
create_error_report <- function(error, include_traceback = TRUE) {
  report <- list(
    message = error$message,
    type = if (!is.null(error$type)) error$type else "UNKNOWN",
    timestamp = if (!is.null(error$timestamp)) error$timestamp else Sys.time(),
    details = error$details,
    recoverable = !is.null(get_recovery_strategy(error$type))
  )
  
  if (include_traceback && !is.null(error$call)) {
    report$traceback <- deparse(error$call)
  }
  
  # Add system context
  report$system_context <- list(
    r_version = R.version.string,
    platform = .Platform$OS.type,
    memory_available = tryCatch({
      gc()[2, 2]  # Available memory in MB
    }, error = function(e) NA)
  )
  
  return(report)
}

#' Save error report to file
#' @param error Error object
#' @param output_dir Output directory
#' @return Path to error report file
save_error_report <- function(error, output_dir = "logs/errors") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  report <- create_error_report(error)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  report_file <- file.path(output_dir, paste0("error_", timestamp, ".rds"))
  
  tryCatch({
    saveRDS(report, report_file)
    
    # Also save as text for easy reading
    text_file <- gsub("\\.rds$", ".txt", report_file)
    report_text <- sprintf(
      "Error Report\n============\n\nTimestamp: %s\nType: %s\nMessage: %s\n\nDetails:\n%s\n\nSystem Context:\n%s\n",
      report$timestamp,
      report$type,
      report$message,
      if (!is.null(report$details)) paste(capture.output(str(report$details)), collapse = "\n") else "None",
      paste(capture.output(str(report$system_context)), collapse = "\n")
    )
    writeLines(report_text, text_file)
    
    if (exists("log_info", mode = "function")) {
      log_info("Error report saved", file = report_file)
    }
    
    return(report_file)
  }, error = function(e) {
    if (exists("log_error", mode = "function")) {
      log_error(sprintf("Failed to save error report: %s", e$message))
    }
    return(NULL)
  })
}

#' Monitor function execution with error handling
#' @param func Function to monitor
#' @param func_name Function name
#' @param error_handler Custom error handler
#' @param ... Arguments to pass to function
#' @return Function result or error
monitor_execution <- function(func, func_name = deparse(substitute(func)), 
                              error_handler = NULL, ...) {
  start_time <- Sys.time()
  start_memory <- gc()[2, 2]
  
  if (exists("log_debug", mode = "function")) {
    log_debug(sprintf("Starting monitored execution: %s", func_name))
  }
  
  result <- tryCatch({
    func(...)
  }, error = function(e) {
    # Create structured error
    err <- create_error(
      message = e$message,
      type = ERROR_TYPES$SYSTEM_ERROR,
      details = list(
        function_name = func_name,
        call = sys.call(-1)
      )
    )
    
    # Save error report
    save_error_report(err)
    
    # Try recovery if handler provided
    if (!is.null(error_handler)) {
      recovery_result <- handle_error(err, error_handler)
      if (!is.null(recovery_result)) {
        return(recovery_result)
      }
    }
    
    # Re-throw error
    stop(err)
  }, finally = {
    # Log execution metrics
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    end_memory <- gc()[2, 2]
    memory_used <- start_memory - end_memory
    
    if (exists("log_performance", mode = "function")) {
      log_performance(func_name, list(
        elapsed_seconds = round(elapsed, 3),
        memory_mb = round(memory_used, 2)
      ))
    }
  })
  
  return(result)
}

#' Batch error handler for multiple operations
#' @param operations List of operations to execute
#' @param continue_on_error Whether to continue after errors
#' @return List of results and errors
batch_execute_with_errors <- function(operations, continue_on_error = TRUE) {
  results <- list()
  errors <- list()
  
  for (i in seq_along(operations)) {
    op_name <- names(operations)[i]
    if (is.null(op_name) || op_name == "") {
      op_name <- sprintf("Operation_%d", i)
    }
    
    result <- tryCatch({
      operations[[i]]()
    }, error = function(e) {
      err <- create_error(
        message = e$message,
        type = ERROR_TYPES$SYSTEM_ERROR,
        details = list(operation = op_name, index = i)
      )
      
      errors[[op_name]] <<- err
      
      if (exists("log_error", mode = "function")) {
        log_error(sprintf("Error in %s: %s", op_name, e$message))
      }
      
      if (!continue_on_error) {
        stop(err)
      }
      
      return(NULL)
    })
    
    results[[op_name]] <- result
  }
  
  return(list(
    results = results,
    errors = errors,
    success_count = sum(sapply(results, function(x) !is.null(x))),
    error_count = length(errors)
  ))
}

#' Get error statistics
#' @param error_dir Directory containing error reports
#' @return Data frame of error statistics
get_error_statistics <- function(error_dir = "logs/errors") {
  if (!dir.exists(error_dir)) {
    return(data.frame())
  }
  
  error_files <- list.files(error_dir, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(error_files) == 0) {
    return(data.frame())
  }
  
  stats <- lapply(error_files, function(file) {
    tryCatch({
      report <- readRDS(file)
      data.frame(
        timestamp = report$timestamp,
        type = report$type,
        message = substr(report$message, 1, 100),
        recoverable = report$recoverable,
        file = basename(file),
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      NULL
    })
  })
  
  stats <- do.call(rbind, Filter(Negate(is.null), stats))
  
  if (nrow(stats) > 0) {
    stats <- stats[order(stats$timestamp, decreasing = TRUE), ]
  }
  
  return(stats)
}
