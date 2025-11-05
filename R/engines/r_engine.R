# R/lavaan Analysis Engine Adapter
# Implements the unified engine interface for R/lavaan

#' Create R/lavaan engine
#' @param config Engine configuration
#' @return R engine instance
create_r_engine <- function(config = NULL) {
  engine <- create_engine_interface()
  
  # Engine identification
  engine$name <- "R/lavaan"
  engine$type <- "r"
  engine$version <- NULL
  engine$config <- config
  engine$fitted_model <- NULL
  
  # Check availability
  engine$is_available <- function() {
    # Check if lavaan package is installed
    if (!requireNamespace("lavaan", quietly = TRUE)) {
      return(FALSE)
    }
    
    # Check optional packages
    engine$semplot_available <<- requireNamespace("semPlot", quietly = TRUE)
    engine$psych_available <<- requireNamespace("psych", quietly = TRUE)
    
    return(TRUE)
  }
  
  # Initialize engine
  engine$initialize <- function(config = NULL) {
    if (!is.null(config)) {
      engine$config <<- config
    }
    
    # Load required packages
    required_packages <- c("lavaan")
    for (pkg in required_packages) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(sprintf("%s package is required but not installed", pkg))
      }
    }
    
    suppressPackageStartupMessages({
      library(lavaan, quietly = TRUE)
    })
    
    # Load optional packages
    if (engine$semplot_available) {
      suppressPackageStartupMessages({
        library(semPlot, quietly = TRUE)
      })
    }
    
    if (engine$psych_available) {
      suppressPackageStartupMessages({
        library(psych, quietly = TRUE)
      })
    }
    
    # Get lavaan version
    engine$version <<- as.character(packageVersion("lavaan"))
    
    if (exists("log_info", mode = "function")) {
      log_info("R/lavaan engine initialized", 
               version = engine$version,
               semplot = engine$semplot_available,
               psych = engine$psych_available)
    }
    
    return(TRUE)
  }
  
  # Parse model specification
  engine$parse_model <- function(model_spec) {
    if (is.character(model_spec)) {
      # Model spec is already in lavaan syntax
      return(model_spec)
    } else if (is.list(model_spec)) {
      # Convert list specification to lavaan syntax
      return(convert_to_lavaan_syntax(model_spec))
    } else {
      stop("Invalid model specification format")
    }
  }
  
  # Validate model specification
  engine$validate_model <- function(model_spec) {
    parsed_model <- engine$parse_model(model_spec)
    
    errors <- character()
    warnings <- character()
    
    # Try to parse the model syntax
    tryCatch({
      lavaan::lavaanify(parsed_model)
    }, error = function(e) {
      errors <<- c(errors, sprintf("Syntax error: %s", e$message))
    }, warning = function(w) {
      warnings <<- c(warnings, w$message)
    })
    
    # Check for common issues
    if (grepl("~.*~", parsed_model)) {
      warnings <- c(warnings, "Multiple regression operators detected - check syntax")
    }
    
    return(list(
      valid = length(errors) == 0,
      errors = errors,
      warnings = warnings
    ))
  }
  
  # Prepare data
  engine$prepare_data <- function(data, data_path = NULL) {
    # For R engine, data is used directly
    # Just validate it's a data frame
    if (!is.data.frame(data)) {
      tryCatch({
        data <- as.data.frame(data)
      }, error = function(e) {
        stop("Data must be convertible to data frame")
      })
    }
    
    if (exists("log_info", mode = "function")) {
      log_info("Data prepared for R/lavaan", nrow = nrow(data), ncol = ncol(data))
    }
    
    return(data)
  }
  
  # Run single model
  engine$run_model <- function(model_spec, data_path, output_path = NULL) {
    # For R engine, data_path can be either a path or data frame
    data <- if (is.character(data_path) && file.exists(data_path)) {
      read_data_file(data_path)
    } else if (is.data.frame(data_path)) {
      data_path
    } else {
      stop("data_path must be a file path or data frame")
    }
    
    # Parse model
    parsed_model <- engine$parse_model(model_spec)
    
    # Get estimation options from config
    estimator <- if (!is.null(engine$config$r$default_estimator)) {
      engine$config$r$default_estimator
    } else {
      "ML"
    }
    
    missing <- "fiml"  # Full Information Maximum Likelihood for missing data
    
    if (exists("log_info", mode = "function")) {
      log_info("Running lavaan model", estimator = estimator)
    }
    
    # Fit model
    tryCatch({
      fit <- lavaan::sem(
        model = parsed_model,
        data = data,
        estimator = estimator,
        missing = missing,
        fixed.x = FALSE
      )
      
      engine$fitted_model <<- fit
      
      # Save results if output path provided
      if (!is.null(output_path)) {
        saveRDS(fit, output_path)
        
        # Also save summary as text
        text_output <- gsub("\\.rds$", ".txt", output_path)
        sink(text_output)
        print(summary(fit, fit.measures = TRUE, standardized = TRUE))
        sink()
      }
      
      if (exists("log_info", mode = "function")) {
        log_info("lavaan model completed successfully")
      }
      
      return(fit)
    }, error = function(e) {
      if (exists("log_error", mode = "function")) {
        log_error(sprintf("lavaan execution failed: %s", e$message))
      }
      stop(e)
    })
  }
  
  # Run batch of models
  engine$run_batch <- function(model_specs, data_path, output_dir = NULL) {
    if (is.null(output_dir)) {
      output_dir <- file.path(tempdir(), "lavaan_batch")
    }
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Load data once
    data <- if (is.character(data_path) && file.exists(data_path)) {
      read_data_file(data_path)
    } else if (is.data.frame(data_path)) {
      data_path
    } else {
      stop("data_path must be a file path or data frame")
    }
    
    results <- list()
    n_models <- length(model_specs)
    
    if (exists("log_info", mode = "function")) {
      log_info(sprintf("Starting batch execution of %d models", n_models))
    }
    
    # Create progress bar
    pb <- create_progress_bar(n_models, "Running lavaan models")
    
    # Parallel processing if enabled
    use_parallel <- !is.null(engine$config$r$parallel_processing) && 
                    engine$config$r$parallel_processing
    
    if (use_parallel && requireNamespace("future", quietly = TRUE)) {
      future::plan(future::multisession)
      
      results <- future.apply::future_lapply(seq_along(model_specs), function(i) {
        model_name <- names(model_specs)[i]
        if (is.null(model_name) || model_name == "") {
          model_name <- sprintf("model_%03d", i)
        }
        
        output_path <- file.path(output_dir, paste0(model_name, ".rds"))
        
        tryCatch({
          fit <- engine$run_model(model_specs[[i]], data, output_path)
          list(success = TRUE, fit = fit, error = NULL)
        }, error = function(e) {
          list(success = FALSE, fit = NULL, error = e$message)
        })
      })
      
      names(results) <- names(model_specs)
    } else {
      # Sequential processing
      for (i in seq_along(model_specs)) {
        model_name <- names(model_specs)[i]
        if (is.null(model_name) || model_name == "") {
          model_name <- sprintf("model_%03d", i)
        }
        
        output_path <- file.path(output_dir, paste0(model_name, ".rds"))
        
        result <- tryCatch({
          fit <- engine$run_model(model_specs[[i]], data, output_path)
          list(success = TRUE, fit = fit, error = NULL)
        }, error = function(e) {
          list(success = FALSE, fit = NULL, error = e$message)
        })
        
        results[[model_name]] <- result
        update_progress(pb)
      }
    }
    
    success_count <- sum(sapply(results, function(x) x$success))
    
    if (exists("log_info", mode = "function")) {
      log_info(sprintf("Batch execution completed: %d/%d successful", 
                      success_count, n_models))
    }
    
    return(results)
  }
  
  # Extract results
  engine$extract_results <- function(output_path) {
    fit <- if (is.character(output_path) && file.exists(output_path)) {
      readRDS(output_path)
    } else if (inherits(output_path, "lavaan")) {
      output_path
    } else {
      stop("output_path must be a file path or lavaan object")
    }
    
    # Standardize results
    standard_results <- standardize_lavaan_results(fit)
    
    return(standard_results)
  }
  
  # Extract fit indices
  engine$extract_fit_indices <- function(output_path) {
    results <- engine$extract_results(output_path)
    return(results$fit_indices)
  }
  
  # Extract parameters
  engine$extract_parameters <- function(output_path) {
    results <- engine$extract_results(output_path)
    return(results$parameters)
  }
  
  # Generate diagram
  engine$generate_diagram <- function(model_spec, output_path = NULL) {
    if (!engine$semplot_available) {
      warning("semPlot package not available. Install it for diagram generation.")
      return(NULL)
    }
    
    # Get fitted model
    fit <- if (inherits(model_spec, "lavaan")) {
      model_spec
    } else if (!is.null(engine$fitted_model)) {
      engine$fitted_model
    } else {
      stop("No fitted model available. Run model first or provide lavaan object.")
    }
    
    # Generate diagram
    tryCatch({
      if (!is.null(output_path)) {
        # Save to file
        if (grepl("\\.pdf$", output_path, ignore.case = TRUE)) {
          pdf(output_path, width = 10, height = 8)
        } else if (grepl("\\.png$", output_path, ignore.case = TRUE)) {
          png(output_path, width = 10, height = 8, units = "in", res = 300)
        } else {
          png(paste0(tools::file_path_sans_ext(output_path), ".png"), 
              width = 10, height = 8, units = "in", res = 300)
        }
      }
      
      semPlot::semPaths(
        fit,
        what = "std",
        layout = "tree",
        edge.label.cex = 0.8,
        curvePivot = TRUE,
        fade = FALSE,
        residuals = FALSE,
        intercepts = FALSE,
        thresholds = FALSE,
        intStyle = "multi"
      )
      
      if (!is.null(output_path)) {
        dev.off()
        
        if (exists("log_info", mode = "function")) {
          log_info("Diagram generated", file = output_path)
        }
      }
      
      return(output_path)
    }, error = function(e) {
      if (exists("log_error", mode = "function")) {
        log_error(sprintf("Failed to generate diagram: %s", e$message))
      }
      return(NULL)
    })
  }
  
  # Cleanup
  engine$cleanup <- function() {
    # Clean up temporary files
    temp_files <- list.files(tempdir(), pattern = "^lavaan_.*\\.(rds|txt)$", 
                             full.names = TRUE)
    if (length(temp_files) > 0) {
      file.remove(temp_files)
      
      if (exists("log_info", mode = "function")) {
        log_info(sprintf("Cleaned up %d temporary lavaan files", length(temp_files)))
      }
    }
    
    # Clear fitted model
    engine$fitted_model <<- NULL
    
    return(TRUE)
  }
  
  class(engine) <- c("REngine", "AnalysisEngine", "list")
  return(engine)
}

#' Standardize lavaan results to common format
#' @param fit lavaan fitted model
#' @return Standardized results list
standardize_lavaan_results <- function(fit) {
  standard_results <- standardize_results(fit, "r")
  
  # Extract fit indices
  fit_measures <- lavaan::fitMeasures(fit)
  
  standard_results$fit_indices <- list(
    chisq = fit_measures["chisq"],
    df = fit_measures["df"],
    pvalue = fit_measures["pvalue"],
    cfi = fit_measures["cfi"],
    tli = fit_measures["tli"],
    rmsea = fit_measures["rmsea"],
    rmsea_ci_lower = fit_measures["rmsea.ci.lower"],
    rmsea_ci_upper = fit_measures["rmsea.ci.upper"],
    srmr = fit_measures["srmr"],
    aic = fit_measures["aic"],
    bic = fit_measures["bic"]
  )
  
  # Extract parameters
  params <- lavaan::parameterEstimates(fit, standardized = TRUE)
  
  standard_results$parameters <- data.frame(
    label = paste(params$lhs, params$op, params$rhs, sep = " "),
    estimate = params$est,
    se = params$se,
    z_value = params$z,
    pvalue = params$pvalue,
    ci_lower = params$ci.lower,
    ci_upper = params$ci.upper,
    std_estimate = params$std.all,
    stringsAsFactors = FALSE
  )
  
  # Extract convergence status
  standard_results$convergence <- lavaan::lavInspect(fit, "converged")
  
  # Extract warnings
  if (length(fit@Fit@warnings) > 0) {
    standard_results$warnings <- fit@Fit@warnings
  }
  
  return(standard_results)
}

#' Convert list specification to lavaan syntax
#' @param spec List specification
#' @return lavaan syntax string
convert_to_lavaan_syntax <- function(spec) {
  syntax <- ""
  
  # Measurement model
  if (!is.null(spec$measurement)) {
    for (latent in names(spec$measurement)) {
      indicators <- spec$measurement[[latent]]
      syntax <- paste0(syntax, sprintf("%s =~ %s\n", latent, paste(indicators, collapse = " + ")))
    }
  }
  
  # Structural model (regressions)
  if (!is.null(spec$regressions)) {
    for (equation in spec$regressions) {
      syntax <- paste0(syntax, equation, "\n")
    }
  }
  
  # Covariances
  if (!is.null(spec$covariances)) {
    for (cov in spec$covariances) {
      syntax <- paste0(syntax, cov, "\n")
    }
  }
  
  # Constraints
  if (!is.null(spec$constraints)) {
    for (constraint in spec$constraints) {
      syntax <- paste0(syntax, constraint, "\n")
    }
  }
  
  return(syntax)
}

#' Read data file (supports multiple formats)
#' @param file_path Path to data file
#' @return Data frame
read_data_file <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))
  
  data <- switch(ext,
    csv = read.csv(file_path, stringsAsFactors = FALSE),
    sav = {
      if (requireNamespace("foreign", quietly = TRUE)) {
        foreign::read.spss(file_path, to.data.frame = TRUE)
      } else {
        stop("foreign package required to read SPSS files")
      }
    },
    xlsx = {
      if (requireNamespace("readxl", quietly = TRUE)) {
        readxl::read_excel(file_path)
      } else {
        stop("readxl package required to read Excel files")
      }
    },
    dta = {
      if (requireNamespace("foreign", quietly = TRUE)) {
        foreign::read.dta(file_path)
      } else {
        stop("foreign package required to read Stata files")
      }
    },
    stop(sprintf("Unsupported file format: %s", ext))
  )
  
  return(as.data.frame(data))
}

# Register R engine
register_engine("r", create_r_engine)
