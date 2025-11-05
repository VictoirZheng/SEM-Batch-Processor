# Mplus Analysis Engine Adapter
# Implements the unified engine interface for Mplus

#' Create Mplus engine
#' @param config Engine configuration
#' @return Mplus engine instance
create_mplus_engine <- function(config = NULL) {
  engine <- create_engine_interface()
  
  # Engine identification
  engine$name <- "Mplus"
  engine$type <- "mplus"
  engine$version <- NULL
  engine$executable_path <- NULL
  engine$config <- config
  
  # Check availability
  engine$is_available <- function() {
    # Check if MplusAutomation package is installed
    if (!requireNamespace("MplusAutomation", quietly = TRUE)) {
      warning("MplusAutomation package is not installed. Install with: install.packages('MplusAutomation')")
      return(FALSE)
    }
    
    # For now, return TRUE if package is available
    # Mplus executable check will be done when actually running
    return(TRUE)
    
    # Check if Mplus executable is available (optional)
    mplus_path <- Sys.which("mplus")
    if (mplus_path == "") {
      # Try common installation paths on Windows
      if (.Platform$OS.type == "windows") {
        common_paths <- c(
          "C:/Program Files/Mplus/mplus.exe",
          "C:/Program Files (x86)/Mplus/mplus.exe"
        )
        for (path in common_paths) {
          if (file.exists(path)) {
            engine$executable_path <<- path
            return(TRUE)
          }
        }
      }
      warning("Mplus executable not found. Please ensure Mplus is installed.")
      return(FALSE)
    }
    
    engine$executable_path <<- as.character(mplus_path)
    return(TRUE)
  }
  
  # Initialize engine
  engine$initialize <- function(config = NULL) {
    if (!is.null(config)) {
      engine$config <<- config
    }
    
    # Load MplusAutomation
    if (!requireNamespace("MplusAutomation", quietly = TRUE)) {
      stop("MplusAutomation package is required but not installed")
    }
    
    suppressPackageStartupMessages({
      library(MplusAutomation, quietly = TRUE)
    })
    
    # Get Mplus version
    tryCatch({
      version_output <- system("mplus -v", intern = TRUE, ignore.stderr = TRUE)
      if (length(version_output) > 0) {
        engine$version <<- gsub(".*Version ([0-9.]+).*", "\\1", version_output[1])
      }
    }, error = function(e) {
      engine$version <<- "Unknown"
    })
    
    if (exists("log_info", mode = "function")) {
      log_info("Mplus engine initialized", version = engine$version)
    }
    
    return(TRUE)
  }
  
  # Parse model specification
  engine$parse_model <- function(model_spec) {
    if (is.character(model_spec)) {
      # Model spec is already in Mplus syntax (with or without placeholders)
      return(model_spec)
    } else if (is.list(model_spec)) {
      # If it's a list with 'template' and 'mapping', apply mapping
      if (!is.null(model_spec$template) && !is.null(model_spec$mapping)) {
        return(apply_mapping(model_spec$template, model_spec$mapping))
      }
      # Otherwise try to convert to Mplus syntax
      return(convert_to_mplus_syntax(model_spec))
    } else {
      stop("Invalid model specification format")
    }
  }
  
  # Validate model specification
  engine$validate_model <- function(model_spec) {
    parsed_model <- engine$parse_model(model_spec)
    
    # Basic syntax validation
    errors <- character()
    warnings <- character()
    
    # Check for required sections
    required_sections <- c("VARIABLE:", "MODEL:")
    for (section in required_sections) {
      if (!grepl(section, parsed_model, ignore.case = TRUE)) {
        errors <- c(errors, sprintf("Missing required section: %s", section))
      }
    }
    
    # Check for common syntax errors
    if (grepl("\\bON\\b.*\\bON\\b", parsed_model)) {
      warnings <- c(warnings, "Multiple ON statements detected - check syntax")
    }
    
    return(list(
      valid = length(errors) == 0,
      errors = errors,
      warnings = warnings
    ))
  }
  
  # Prepare data for Mplus
  engine$prepare_data <- function(data, data_path = NULL) {
    if (is.null(data_path)) {
      data_path <- file.path(tempdir(), "mplus_data.dat")
    }
    
    # Ensure data_path has .dat extension
    if (!grepl("\\.dat$", data_path, ignore.case = TRUE)) {
      data_path <- paste0(tools::file_path_sans_ext(data_path), ".dat")
    }
    
    tryCatch({
      MplusAutomation::prepareMplusData(
        data,
        filename = data_path,
        inpfile = FALSE
      )
      
      if (exists("log_info", mode = "function")) {
        log_info("Data prepared for Mplus", file = data_path, nrow = nrow(data), ncol = ncol(data))
      }
      
      return(data_path)
    }, error = function(e) {
      if (exists("log_error", mode = "function")) {
        log_error(sprintf("Failed to prepare data: %s", e$message))
      }
      stop(e)
    })
  }
  
  # Run single model
  engine$run_model <- function(model_spec, data_path, output_path = NULL) {
    if (is.null(output_path)) {
      output_path <- file.path(tempdir(), "mplus_model.inp")
    }
    
    # Ensure output_path has .inp extension
    if (!grepl("\\.inp$", output_path, ignore.case = TRUE)) {
      output_path <- paste0(tools::file_path_sans_ext(output_path), ".inp")
    }
    
    # Parse model
    parsed_model <- engine$parse_model(model_spec)
    
    # Create complete Mplus input file
    input_content <- create_mplus_input(parsed_model, data_path, engine$config)
    
    # Write input file
    writeLines(input_content, output_path)
    
    if (exists("log_info", mode = "function")) {
      log_info("Running Mplus model", input_file = output_path)
    }
    
    # Run model
    tryCatch({
      MplusAutomation::runModels(output_path, showOutput = FALSE)
      
      # Check if output file was created
      output_file <- gsub("\\.inp$", ".out", output_path)
      if (!file.exists(output_file)) {
        stop("Mplus did not generate output file")
      }
      
      if (exists("log_info", mode = "function")) {
        log_info("Mplus model completed", output_file = output_file)
      }
      
      return(output_file)
    }, error = function(e) {
      if (exists("log_error", mode = "function")) {
        log_error(sprintf("Mplus execution failed: %s", e$message))
      }
      stop(e)
    })
  }
  
  # Run batch of models
  engine$run_batch <- function(model_specs, data_path, output_dir = NULL) {
    if (is.null(output_dir)) {
      output_dir <- file.path(tempdir(), "mplus_batch")
    }
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    results <- list()
    n_models <- length(model_specs)
    
    if (exists("log_info", mode = "function")) {
      log_info(sprintf("Starting batch execution of %d models", n_models))
    }
    
    # Create progress bar
    pb <- create_progress_bar(n_models, "Running Mplus models")
    
    for (i in seq_along(model_specs)) {
      model_name <- names(model_specs)[i]
      if (is.null(model_name) || model_name == "") {
        model_name <- sprintf("model_%03d", i)
      }
      
      output_path <- file.path(output_dir, paste0(model_name, ".inp"))
      
      result <- tryCatch({
        output_file <- engine$run_model(model_specs[[i]], data_path, output_path)
        list(success = TRUE, output_file = output_file, error = NULL)
      }, error = function(e) {
        list(success = FALSE, output_file = NULL, error = e$message)
      })
      
      results[[model_name]] <- result
      update_progress(pb)
    }
    
    success_count <- sum(sapply(results, function(x) x$success))
    
    if (exists("log_info", mode = "function")) {
      log_info(sprintf("Batch execution completed: %d/%d successful", 
                      success_count, n_models))
    }
    
    return(results)
  }
  
  # Extract results from output file
  engine$extract_results <- function(output_path) {
    if (!file.exists(output_path)) {
      stop(sprintf("Output file not found: %s", output_path))
    }
    
    tryCatch({
      results <- MplusAutomation::readModels(output_path)
      
      # Standardize results
      standard_results <- standardize_mplus_results(results)
      
      return(standard_results)
    }, error = function(e) {
      if (exists("log_error", mode = "function")) {
        log_error(sprintf("Failed to extract results: %s", e$message))
      }
      stop(e)
    })
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
  
  # Generate diagram (limited support for Mplus)
  engine$generate_diagram <- function(model_spec, output_path = NULL) {
    warning("Diagram generation is limited for Mplus. Consider using R engine for better visualization.")
    
    # Basic diagram generation using model specification
    # This is a placeholder - actual implementation would be more complex
    return(NULL)
  }
  
  # Cleanup
  engine$cleanup <- function() {
    # Clean up temporary files
    temp_files <- list.files(tempdir(), pattern = "^mplus_.*\\.(dat|inp|out)$", 
                             full.names = TRUE)
    if (length(temp_files) > 0) {
      file.remove(temp_files)
      
      if (exists("log_info", mode = "function")) {
        log_info(sprintf("Cleaned up %d temporary Mplus files", length(temp_files)))
      }
    }
    
    return(TRUE)
  }
  
  class(engine) <- c("MplusEngine", "AnalysisEngine", "list")
  return(engine)
}

#' Create complete Mplus input file content
#' @param model_syntax Model syntax
#' @param data_path Path to data file
#' @param config Engine configuration
#' @return Complete input file content
create_mplus_input <- function(model_syntax, data_path, config = NULL) {
  # Use absolute path for data file to avoid path issues
  data_file_abs <- normalizePath(data_path, winslash = "/", mustWork = FALSE)
  
  # Build input file sections
  title_section <- "TITLE: SEM Analysis\n"
  
  data_section <- sprintf("DATA: FILE = \"%s\";\n", data_file_abs)
  
  # Add model syntax (should already include VARIABLE and MODEL sections)
  input_content <- paste0(title_section, "\n", data_section, "\n", model_syntax)
  
  # Add analysis options if provided in config
  if (!is.null(config) && !is.null(config$mplus)) {
    if (!grepl("ANALYSIS:", input_content, ignore.case = TRUE)) {
      analysis_section <- "\nANALYSIS:\n"
      
      if (!is.null(config$mplus$max_iterations)) {
        analysis_section <- paste0(analysis_section, 
                                   sprintf("  ITERATIONS = %d;\n", config$mplus$max_iterations))
      }
      
      if (!is.null(config$mplus$convergence_criterion)) {
        analysis_section <- paste0(analysis_section,
                                   sprintf("  CONVERGENCE = %.8f;\n", config$mplus$convergence_criterion))
      }
      
      input_content <- paste0(input_content, analysis_section)
    }
  }
  
  # Add output section if not present
  if (!grepl("OUTPUT:", input_content, ignore.case = TRUE)) {
    output_section <- "\nOUTPUT: STANDARDIZED CINTERVAL;\n"
    input_content <- paste0(input_content, output_section)
  }
  
  return(input_content)
}

#' Standardize Mplus results to common format
#' @param mplus_results Raw Mplus results from readModels
#' @return Standardized results list
standardize_mplus_results <- function(mplus_results) {
  # Check if mplus_results is valid
  if (is.null(mplus_results)) {
    stop("Mplus results is NULL")
  }
  
  # Initialize with default structure
  standard_results <- list(
    engine = "mplus",
    timestamp = Sys.time(),
    fit_indices = list(),
    parameters = data.frame(),
    warnings = character(),
    errors = character(),
    convergence = FALSE
  )
  
  # Extract fit indices
  if (!is.null(mplus_results$summaries)) {
    summaries <- mplus_results$summaries
    standard_results$fit_indices <- list(
      chisq = if (!is.null(summaries$ChiSqM_Value)) summaries$ChiSqM_Value else NA,
      df = if (!is.null(summaries$ChiSqM_DF)) summaries$ChiSqM_DF else NA,
      pvalue = if (!is.null(summaries$ChiSqM_PValue)) summaries$ChiSqM_PValue else NA,
      cfi = if (!is.null(summaries$CFI)) summaries$CFI else NA,
      tli = if (!is.null(summaries$TLI)) summaries$TLI else NA,
      rmsea = if (!is.null(summaries$RMSEA_Estimate)) summaries$RMSEA_Estimate else NA,
      rmsea_ci_lower = if (!is.null(summaries$RMSEA_90CI_LB)) summaries$RMSEA_90CI_LB else NA,
      rmsea_ci_upper = if (!is.null(summaries$RMSEA_90CI_UB)) summaries$RMSEA_90CI_UB else NA,
      srmr = if (!is.null(summaries$SRMR)) summaries$SRMR else NA,
      aic = if (!is.null(summaries$AIC)) summaries$AIC else NA,
      bic = if (!is.null(summaries$BIC)) summaries$BIC else NA
    )
  }
  
  # Extract parameters
  if (!is.null(mplus_results$parameters)) {
    if (!is.null(mplus_results$parameters$unstandardized)) {
      params <- mplus_results$parameters$unstandardized
      
      standard_results$parameters <- data.frame(
        label = paste(params$paramHeader, params$param, sep = "_"),
        estimate = params$est,
        se = params$se,
        z_value = params$est / params$se,
        pvalue = params$pval,
        ci_lower = params$lower_2.5ci,
        ci_upper = params$upper_2.5ci,
        std_estimate = if (!is.null(mplus_results$parameters$stdyx.standardized)) {
          mplus_results$parameters$stdyx.standardized$est
        } else {
          NA
        },
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Extract convergence status
  if (!is.null(mplus_results$summaries)) {
    standard_results$convergence <- mplus_results$summaries$LL_converged
  }
  
  # Extract warnings and errors
  if (!is.null(mplus_results$warnings)) {
    standard_results$warnings <- mplus_results$warnings
  }
  
  if (!is.null(mplus_results$errors)) {
    standard_results$errors <- mplus_results$errors
  }
  
  return(standard_results)
}

#' Convert list specification to Mplus syntax
#' @param spec List specification
#' @return Mplus syntax string
convert_to_mplus_syntax <- function(spec) {
  # This is a placeholder for converting structured specifications to Mplus syntax
  # Actual implementation would be more complex
  
  syntax <- ""
  
  if (!is.null(spec$variables)) {
    syntax <- paste0(syntax, "VARIABLE:\n")
    syntax <- paste0(syntax, sprintf("  NAMES = %s;\n", paste(spec$variables, collapse = " ")))
    
    if (!is.null(spec$usevariables)) {
      syntax <- paste0(syntax, sprintf("  USEVARIABLES = %s;\n", 
                                      paste(spec$usevariables, collapse = " ")))
    }
  }
  
  if (!is.null(spec$model)) {
    syntax <- paste0(syntax, "\nMODEL:\n")
    for (equation in spec$model) {
      syntax <- paste0(syntax, sprintf("  %s;\n", equation))
    }
  }
  
  return(syntax)
}

# Register Mplus engine
register_engine("mplus", create_mplus_engine)
