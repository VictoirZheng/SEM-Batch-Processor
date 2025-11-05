# Engine Manager
# Manages engine detection, selection, and switching

#' Engine Manager class
#' @description Manages multiple analysis engines and provides switching capabilities
create_engine_manager <- function() {
  manager <- list(
    current_engine = NULL,
    available_engines = list(),
    engine_history = list(),
    config = NULL
  )
  
  #' Initialize engine manager
  manager$initialize <- function(config = NULL) {
    manager$config <<- config
    
    # Detect available engines
    manager$detect_engines()
    
    # Set default engine
    preference <- if (!is.null(config$engines$default)) {
      config$engines$default
    } else {
      "auto"
    }
    
    manager$set_engine(preference)
    
    if (exists("log_info", mode = "function")) {
      log_info("Engine manager initialized", 
               current_engine = manager$current_engine$name,
               available_count = length(manager$available_engines))
    }
    
    return(TRUE)
  }
  
  #' Detect available engines
  manager$detect_engines <- function() {
    detection_results <- detect_available_engines()
    
    manager$available_engines <<- list()
    
    # Try to create each available engine
    for (engine_name in names(detection_results)) {
      if (detection_results[[engine_name]]$available) {
        tryCatch({
          engine <- create_engine(engine_name, manager$config)
          if (engine$is_available()) {
            engine$initialize(manager$config)
            manager$available_engines[[engine_name]] <<- engine
          }
        }, error = function(e) {
          if (exists("log_warning", mode = "function")) {
            log_warning(sprintf("Failed to initialize %s engine: %s", 
                               engine_name, e$message))
          }
        })
      }
    }
    
    if (exists("log_info", mode = "function")) {
      log_info(sprintf("Detected %d available engines", 
                      length(manager$available_engines)))
    }
    
    return(manager$available_engines)
  }
  
  #' Set current engine
  manager$set_engine <- function(engine_preference) {
    # Determine which engine to use
    engine_name <- if (engine_preference == "auto") {
      get_recommended_engine(engine_preference)
    } else {
      engine_preference
    }
    
    # Check if engine is available
    if (!engine_name %in% names(manager$available_engines)) {
      stop(sprintf("Engine '%s' is not available. Available engines: %s",
                   engine_name, paste(names(manager$available_engines), collapse = ", ")))
    }
    
    # Save previous engine to history
    if (!is.null(manager$current_engine)) {
      manager$engine_history <<- c(manager$engine_history, 
                                   list(list(
                                     engine = manager$current_engine,
                                     timestamp = Sys.time()
                                   )))
    }
    
    # Set new engine
    manager$current_engine <<- manager$available_engines[[engine_name]]
    
    if (exists("log_info", mode = "function")) {
      log_info(sprintf("Engine set to: %s", manager$current_engine$name))
    }
    
    return(manager$current_engine)
  }
  
  #' Get current engine
  manager$get_engine <- function() {
    if (is.null(manager$current_engine)) {
      stop("No engine is currently set. Call initialize() first.")
    }
    return(manager$current_engine)
  }
  
  #' Switch to different engine
  manager$switch_engine <- function(new_engine_name) {
    if (exists("log_info", mode = "function")) {
      log_info(sprintf("Switching engine from %s to %s", 
                      manager$current_engine$name, new_engine_name))
    }
    
    return(manager$set_engine(new_engine_name))
  }
  
  #' Get engine status
  manager$get_status <- function() {
    status <- list(
      current_engine = if (!is.null(manager$current_engine)) {
        manager$current_engine$name
      } else {
        NULL
      },
      available_engines = names(manager$available_engines),
      engine_details = lapply(manager$available_engines, function(eng) {
        list(
          name = eng$name,
          type = eng$type,
          version = eng$version,
          available = eng$is_available()
        )
      })
    )
    
    return(status)
  }
  
  #' Run model with automatic fallback
  manager$run_with_fallback <- function(model_spec, data_path, output_path = NULL) {
    primary_engine <- manager$current_engine
    
    result <- tryCatch({
      primary_engine$run_model(model_spec, data_path, output_path)
    }, error = function(e) {
      if (exists("log_warning", mode = "function")) {
        log_warning(sprintf("Primary engine (%s) failed: %s", 
                           primary_engine$name, e$message))
      }
      
      # Try fallback engines
      for (engine_name in names(manager$available_engines)) {
        if (engine_name != primary_engine$type) {
          if (exists("log_info", mode = "function")) {
            log_info(sprintf("Attempting fallback to %s engine", engine_name))
          }
          
          fallback_result <- tryCatch({
            fallback_engine <- manager$available_engines[[engine_name]]
            fallback_engine$run_model(model_spec, data_path, output_path)
          }, error = function(e2) {
            if (exists("log_warning", mode = "function")) {
              log_warning(sprintf("Fallback engine (%s) also failed: %s", 
                                 engine_name, e2$message))
            }
            NULL
          })
          
          if (!is.null(fallback_result)) {
            if (exists("log_info", mode = "function")) {
              log_info(sprintf("Successfully ran model with fallback engine: %s", 
                              engine_name))
            }
            return(fallback_result)
          }
        }
      }
      
      # All engines failed
      stop(sprintf("All engines failed to run model. Primary error: %s", e$message))
    })
    
    return(result)
  }
  
  #' Compare results across engines
  manager$compare_engines <- function(model_spec, data_path) {
    if (length(manager$available_engines) < 2) {
      warning("Need at least 2 engines for comparison")
      return(NULL)
    }
    
    results <- list()
    
    for (engine_name in names(manager$available_engines)) {
      if (exists("log_info", mode = "function")) {
        log_info(sprintf("Running model with %s engine for comparison", engine_name))
      }
      
      engine <- manager$available_engines[[engine_name]]
      
      result <- tryCatch({
        output_path <- file.path(tempdir(), paste0("compare_", engine_name, ".out"))
        fit <- engine$run_model(model_spec, data_path, output_path)
        extracted <- engine$extract_results(fit)
        
        list(
          success = TRUE,
          fit_indices = extracted$fit_indices,
          parameters = extracted$parameters,
          convergence = extracted$convergence,
          error = NULL
        )
      }, error = function(e) {
        list(
          success = FALSE,
          fit_indices = NULL,
          parameters = NULL,
          convergence = FALSE,
          error = e$message
        )
      })
      
      results[[engine_name]] <- result
    }
    
    # Create comparison summary
    comparison <- create_engine_comparison_summary(results)
    
    return(comparison)
  }
  
  #' Get engine recommendations
  manager$get_recommendations <- function(model_spec = NULL, data = NULL) {
    recommendations <- list()
    
    for (engine_name in names(manager$available_engines)) {
      engine <- manager$available_engines[[engine_name]]
      
      score <- 0
      reasons <- character()
      
      # Base availability score
      if (engine$is_available()) {
        score <- score + 10
        reasons <- c(reasons, "Engine is available")
      }
      
      # Engine-specific advantages
      if (engine$type == "mplus") {
        score <- score + 5
        reasons <- c(reasons, "Mplus: Industry standard, robust convergence")
        
        # Check for features that Mplus handles better
        if (!is.null(model_spec)) {
          if (grepl("CATEGORICAL|TYPE.*MIXTURE", model_spec, ignore.case = TRUE)) {
            score <- score + 10
            reasons <- c(reasons, "Mplus: Better for categorical/mixture models")
          }
        }
      } else if (engine$type == "r") {
        score <- score + 3
        reasons <- c(reasons, "R/lavaan: Open source, flexible, good visualization")
        
        # Check for features that R handles better
        if (engine$semplot_available) {
          score <- score + 5
          reasons <- c(reasons, "R: Excellent diagram generation with semPlot")
        }
        
        if (!is.null(data) && nrow(data) < 1000) {
          score <- score + 3
          reasons <- c(reasons, "R: Good for smaller datasets")
        }
      }
      
      recommendations[[engine_name]] <- list(
        score = score,
        reasons = reasons,
        recommended = score >= 15
      )
    }
    
    # Sort by score
    recommendations <- recommendations[order(sapply(recommendations, function(x) x$score), 
                                            decreasing = TRUE)]
    
    return(recommendations)
  }
  
  #' Clean up all engines
  manager$cleanup_all <- function() {
    for (engine_name in names(manager$available_engines)) {
      tryCatch({
        manager$available_engines[[engine_name]]$cleanup()
      }, error = function(e) {
        if (exists("log_warning", mode = "function")) {
          log_warning(sprintf("Failed to cleanup %s engine: %s", 
                             engine_name, e$message))
        }
      })
    }
    
    if (exists("log_info", mode = "function")) {
      log_info("All engines cleaned up")
    }
    
    return(TRUE)
  }
  
  class(manager) <- c("EngineManager", "list")
  return(manager)
}

#' Create engine comparison summary
#' @param results Results from multiple engines
#' @return Comparison summary
create_engine_comparison_summary <- function(results) {
  summary <- list(
    engines_compared = names(results),
    all_successful = all(sapply(results, function(x) x$success)),
    fit_indices_comparison = NULL,
    parameter_differences = NULL
  )
  
  # Compare fit indices
  if (summary$all_successful) {
    fit_comparison <- data.frame(
      Engine = names(results),
      stringsAsFactors = FALSE
    )
    
    # Extract common fit indices
    fit_indices <- c("chisq", "df", "cfi", "tli", "rmsea", "srmr", "aic", "bic")
    
    for (idx in fit_indices) {
      fit_comparison[[idx]] <- sapply(results, function(r) {
        if (!is.null(r$fit_indices[[idx]])) {
          r$fit_indices[[idx]]
        } else {
          NA
        }
      })
    }
    
    summary$fit_indices_comparison <- fit_comparison
    
    # Calculate differences
    if (nrow(fit_comparison) == 2) {
      differences <- list()
      for (idx in fit_indices) {
        if (all(!is.na(fit_comparison[[idx]]))) {
          diff <- abs(fit_comparison[[idx]][1] - fit_comparison[[idx]][2])
          rel_diff <- diff / mean(fit_comparison[[idx]]) * 100
          differences[[idx]] <- list(
            absolute = diff,
            relative_percent = rel_diff
          )
        }
      }
      summary$parameter_differences <- differences
    }
  }
  
  return(summary)
}

#' Global engine manager instance
.global_engine_manager <- NULL

#' Get global engine manager
#' @return Engine manager instance
get_engine_manager <- function() {
  if (is.null(.global_engine_manager)) {
    .global_engine_manager <<- create_engine_manager()
    
    # Initialize with config if available
    if (exists("get_config", mode = "function")) {
      config <- get_config("engines")
      .global_engine_manager$initialize(config)
    } else {
      .global_engine_manager$initialize()
    }
  }
  
  return(.global_engine_manager)
}

#' Reset global engine manager
reset_engine_manager <- function() {
  if (!is.null(.global_engine_manager)) {
    .global_engine_manager$cleanup_all()
  }
  .global_engine_manager <<- NULL
  invisible(TRUE)
}
