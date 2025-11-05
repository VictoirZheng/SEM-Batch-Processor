# Mplus Batch Processor
# Handles batch Mplus analysis following the mplus_auto workflow

#' Run batch Mplus analysis
#' @param data Data frame
#' @param models List of models with name and syntax
#' @param output_dir Output directory for inp and out files
#' @param progress_callback Progress callback function
#' @return List with results
run_mplus_batch <- function(data, models, output_dir = NULL, progress_callback = NULL) {
  
  if (!requireNamespace("MplusAutomation", quietly = TRUE)) {
    stop("MplusAutomation package is required")
  }
  
  # Create output directory
  if (is.null(output_dir)) {
    output_dir <- file.path(tempdir(), "mplus_batch", format(Sys.time(), "%Y%m%d_%H%M%S"))
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  if (!is.null(progress_callback)) {
    progress_callback(5, "PreparingMplusdata file...")
  }
  
  # Step 1: Prepare data file (once for all models)
  data_file <- file.path(output_dir, "data.dat")
  tryCatch({
    MplusAutomation::prepareMplusData(
      data,
      filename = data_file,
      inpfile = FALSE
    )
    cat(sprintf("✓ Data file created: %s\n", data_file))
  }, error = function(e) {
    stop(sprintf("Failed to prepare data file: %s", e$message))
  })
  
  if (!is.null(progress_callback)) {
    progress_callback(10, sprintf("Generate %d Mplusinput file...", length(models)))
  }
  
  # Step 2: Generate inp files for all models
  inp_files <- character(length(models))
  model_names <- character(length(models))
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    model_name <- model$name
    model_syntax <- model$syntax
    
    # Clean model name for filename
    safe_name <- gsub("[^a-zA-Z0-9_-]", "_", model_name)
    safe_name <- gsub("_{2,}", "_", safe_name)  # Remove multiple underscores
    
    # Create inp file
    inp_file <- file.path(output_dir, paste0(safe_name, ".inp"))
    
    # Build complete Mplus input
    inp_content <- create_mplus_input_content(model_syntax, "data.dat")
    
    # Write inp file
    writeLines(inp_content, inp_file)
    
    inp_files[i] <- inp_file
    model_names[i] <- model_name
    
    cat(sprintf("✓ generated: %s\n", basename(inp_file)))
  }
  
  if (!is.null(progress_callback)) {
    progress_callback(30, sprintf("Batch run %d MplusModel...", length(models)))
  }
  
  # Step 3: Batch run all inp files
  cat(sprintf("\nHomeBatch run %d MplusModel...\n", length(inp_files)))
  
  tryCatch({
    MplusAutomation::runModels(output_dir, showOutput = FALSE, replaceOutfile = "always")
    cat("✓ Batch runCompleted\n")
  }, error = function(e) {
    warning(sprintf("Batch runError occurred: %s", e$message))
  })
  
  if (!is.null(progress_callback)) {
    progress_callback(70, "ReadMplusresults...")
  }
  
  # Step 4: Read all output files
  cat("\nReadMplusresults...\n")
  
  all_results <- list()
  
  for (i in seq_along(inp_files)) {
    model_name <- model_names[i]
    out_file <- gsub("\\.inp$", ".out", inp_files[i])
    
    result <- list(
      model_name = model_name,
      success = FALSE,
      fit_indices = NULL,
      parameters = NULL,
      warnings = character(),
      errors = character()
    )
    
    if (file.exists(out_file)) {
      tryCatch({
        # Read Mplus output
        mplus_output <- MplusAutomation::readModels(out_file)
        
        # Extract fit indices
        if (!is.null(mplus_output$summaries)) {
          summaries <- mplus_output$summaries
          result$fit_indices <- list(
            chisq = summaries$ChiSqM_Value,
            df = summaries$ChiSqM_DF,
            pvalue = summaries$ChiSqM_PValue,
            cfi = summaries$CFI,
            tli = summaries$TLI,
            rmsea = summaries$RMSEA_Estimate,
            rmsea_ci_lower = summaries$RMSEA_90CI_LB,
            rmsea_ci_upper = summaries$RMSEA_90CI_UB,
            srmr = summaries$SRMR,
            aic = summaries$AIC,
            bic = summaries$BIC
          )
        }
        
        # Extract parameters
        if (!is.null(mplus_output$parameters) && !is.null(mplus_output$parameters$unstandardized)) {
          params <- mplus_output$parameters$unstandardized
          result$parameters <- params
        }
        
        # Extract warnings and errors
        if (!is.null(mplus_output$warnings)) {
          result$warnings <- mplus_output$warnings
        }
        
        if (!is.null(mplus_output$errors)) {
          result$errors <- mplus_output$errors
        }
        
        result$success <- TRUE
        cat(sprintf("  ✓ %s\n", model_name))
        
      }, error = function(e) {
        result$errors <<- c(result$errors, e$message)
        cat(sprintf("  ✗ %s: %s\n", model_name, e$message))
      })
    } else {
      result$errors <- c("Output file notGenerate")
      cat(sprintf("  ✗ %s: Output file notGenerate\n", model_name))
    }
    
    all_results[[model_name]] <- result
    
    # Update progress
    if (!is.null(progress_callback)) {
      prog <- 70 + (i / length(inp_files)) * 25
      progress_callback(prog, sprintf("Processed %d/%d Models", i, length(inp_files)))
    }
  }
  
  if (!is.null(progress_callback)) {
    progress_callback(100, "BatchAnalysis completed！")
  }
  
  # Summary
  n_success <- sum(sapply(all_results, function(r) r$success))
  n_failed <- length(all_results) - n_success
  
  cat(sprintf("\nBatchAnalysis completed\n"))
  cat(sprintf("Total number of models: %d\n", length(all_results)))
  cat(sprintf("Success: %d\n", n_success))
  cat(sprintf("Failed: %d\n", n_failed))
  
  return(list(
    success = TRUE,
    output_dir = output_dir,
    n_models = length(all_results),
    n_success = n_success,
    n_failed = n_failed,
    models = all_results
  ))
}

#' Create Mplus input file content with NAMES
#' @param model_syntax Model syntax (should already include VARIABLE and MODEL sections)
#' @param data_file Data file name (relative to inp file)
#' @param all_var_names All variable names in the data
#' @return Complete inp file content
create_mplus_input_with_names <- function(model_syntax, data_file, all_var_names) {
  # Remove any existing TITLE from model_syntax
  model_syntax <- gsub("^TITLE:.*?\n", "", model_syntax, ignore.case = TRUE)
  model_syntax <- trimws(model_syntax)
  
  # Build header sections
  title_section <- "TITLE: SEM Analysis\n"
  data_section <- sprintf("DATA: FILE = \"%s\";\n", data_file)
  
  # Create NAMES line with proper formatting (max 80 chars per line)
  names_lines <- create_names_section(all_var_names)
  
  # Check if model_syntax already has VARIABLE section
  if (grepl("VARIABLE:", model_syntax, ignore.case = TRUE)) {
    # Remove any existing NAMES line
    model_syntax <- gsub("\\s*NAMES\\s*=.*?;", "", model_syntax, ignore.case = TRUE)
    
    # Add NAMES after VARIABLE:
    model_syntax <- gsub(
      "(VARIABLE:)",
      sprintf("\\1\n%s", names_lines),
      model_syntax,
      ignore.case = TRUE
    )
  } else {
    # No VARIABLE section, create one
    variable_section <- sprintf("VARIABLE:\n%s\n", names_lines)
    model_syntax <- paste0(variable_section, model_syntax)
  }
  
  # Combine sections
  inp_content <- paste0(title_section, "\n", data_section, "\n", model_syntax)
  
  # Add OUTPUT section if not present
  if (!grepl("OUTPUT:", inp_content, ignore.case = TRUE)) {
    inp_content <- paste0(inp_content, "\n\nOUTPUT: STANDARDIZED CINTERVAL;\n")
  }
  
  return(inp_content)
}

#' Create NAMES section with proper line breaks
#' @param var_names Variable names
#' @return Formatted NAMES section
create_names_section <- function(var_names) {
  # Mplus has a 90 character line limit, use 80 to be safe
  max_line_length <- 80
  
  names_lines <- character()
  current_line <- "  NAMES ="
  
  for (i in seq_along(var_names)) {
    var <- var_names[i]
    
    # Check if adding this variable would exceed line length
    test_line <- if (i == 1) {
      paste(current_line, var)
    } else {
      paste(current_line, var)
    }
    
    if (nchar(test_line) > max_line_length && current_line != "  NAMES =") {
      # Save current line and start new one
      names_lines <- c(names_lines, current_line)
      current_line <- paste("   ", var)  # Continuation line with extra space
    } else {
      current_line <- test_line
    }
  }
  
  # Add semicolon to last line
  current_line <- paste0(current_line, ";")
  names_lines <- c(names_lines, current_line)
  
  return(paste(names_lines, collapse = "\n"))
}

#' Create Mplus input file content (legacy function for compatibility)
#' @param model_syntax Model syntax
#' @param data_file Data file name
#' @return Complete inp file content
create_mplus_input_content <- function(model_syntax, data_file) {
  # Build complete input file
  title_section <- "TITLE: SEM Analysis\n"
  data_section <- sprintf("DATA: FILE = \"%s\";\n", data_file)
  
  # Combine sections
  inp_content <- paste0(title_section, "\n", data_section, "\n", model_syntax)
  
  # Add OUTPUT section if not present
  if (!grepl("OUTPUT:", inp_content, ignore.case = TRUE)) {
    inp_content <- paste0(inp_content, "\n\nOUTPUT: STANDARDIZED CINTERVAL;\n")
  }
  
  return(inp_content)
}

#' Generate Mplus inp files only (without running)
#' @param data Data frame
#' @param models List of models with name and syntax
#' @param output_dir Output directory
#' @return List with file paths
generate_mplus_files <- function(data, models, output_dir) {
  
  if (!requireNamespace("MplusAutomation", quietly = TRUE)) {
    stop("MplusAutomation package is required")
  }
  
  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat(sprintf("\n=== GenerateMplusFile ===\n"))
  cat(sprintf("Output directory: %s\n", output_dir))
  cat(sprintf("Number of models: %d\n\n", length(models)))
  
  # Step 1: Prepare data file
  data_file <- file.path(output_dir, "data.dat")
  all_var_names <- names(data)  # Store all variable names
  
  tryCatch({
    MplusAutomation::prepareMplusData(
      data,
      filename = data_file,
      inpfile = FALSE
    )
    cat(sprintf("✓ Data file created: %s\n", data_file))
    cat(sprintf("  rowsNumber: %d, columnsNumber: %d\n", nrow(data), ncol(data)))
    cat(sprintf("  Variable: %s\n\n", paste(all_var_names, collapse = " ")))
  }, error = function(e) {
    stop(sprintf("Failed to prepare data file: %s", e$message))
  })
  
  # Step 2: Generate inp files
  inp_files <- character(length(models))
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    model_name <- model$name
    model_syntax <- model$syntax
    
    # Clean model name for filename
    safe_name <- gsub("[^a-zA-Z0-9_-]", "_", model_name)
    safe_name <- gsub("_{2,}", "_", safe_name)
    
    # Create inp file
    inp_file <- file.path(output_dir, paste0(safe_name, ".inp"))
    
    # Build complete Mplus input with NAMES
    inp_content <- create_mplus_input_with_names(model_syntax, "data.dat", all_var_names)
    
    # Write inp file
    writeLines(inp_content, inp_file)
    
    inp_files[i] <- inp_file
    
    cat(sprintf("✓ [%d/%d] %s\n", i, length(models), basename(inp_file)))
  }
  
  cat(sprintf("\n=== FileGenerateCompleted ===\n"))
  cat(sprintf("data file: 1\n"))
  cat(sprintf("input file: %d\n", length(inp_files)))
  cat(sprintf("Location: %s\n\n", output_dir))
  cat(sprintf("Next step: UseMplusOrMplusAutomationRunningTheseinpFile\n"))
  
  return(list(
    success = TRUE,
    data_file = data_file,
    inp_files = inp_files,
    output_dir = output_dir
  ))
}
