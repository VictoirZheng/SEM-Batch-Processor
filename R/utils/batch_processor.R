# Batch Processor
# Handles batch SEM analysis with progress tracking

#' Run batch SEM analysis
#' @param data Data frame with variables
#' @param model_syntax SEM model syntax (with actual variable names)
#' @param engine_mgr Engine manager object
#' @param progress_callback Function to call with progress updates
#' @return List with results
run_batch_analysis <- function(data, model_syntax, engine_mgr, progress_callback = NULL) {
  
  # Initialize results
  results <- list(
    success = FALSE,
    model_syntax = model_syntax,
    fit_indices = NULL,
    parameters = NULL,
    warnings = character(0),
    errors = character(0),
    timestamp = Sys.time()
  )
  
  # Update progress
  if (!is.null(progress_callback)) {
    progress_callback(10, "Preparing analysis...")
  }
  
  # Validate inputs
  if (is.null(data) || nrow(data) == 0) {
    results$errors <- c(results$errors, "Data is empty")
    return(results)
  }
  
  if (is.null(model_syntax) || nchar(trimws(model_syntax)) == 0) {
    results$errors <- c(results$errors, "Model Syntaxis empty")
    return(results)
  }
  
  if (!is.null(progress_callback)) {
    progress_callback(20, "ValidationModel Syntax...")
  }
  
  # Try to run analysis
  tryCatch({
    # Get current engine (robust handling of NULL/empty)
    current_engine <- tryCatch(engine_mgr$current_engine, error = function(e) NULL)
    engine_name <- tryCatch(current_engine$name, error = function(e) NULL)
    if (is.null(engine_name) || length(engine_name) == 0) {
      engine_name <- "R/lavaan"
    }
    
    if (!is.null(progress_callback)) {
      msg <- tryCatch(sprintf("Running %s Analysis...", engine_name), error = function(e) "Running analysis...")
      progress_callback(40, msg)
    }
    
    # Use engine to run analysis
    if (engine_name == "Mplus") {
      # Mplus engine workflow
      if (!is.null(progress_callback)) {
        progress_callback(45, "PreparingMplusdata file...")
      }
      
      # Prepare data file
      data_path <- current_engine$prepare_data(data, file.path(tempdir(), "mplus_data.dat"))
      
      if (!is.null(progress_callback)) {
        progress_callback(50, "GenerateMplusinput file...")
      }
      
      # Run model
      output_file <- current_engine$run_model(
        model_syntax, 
        data_path, 
        file.path(tempdir(), "mplus_model.inp")
      )
      
      if (!is.null(progress_callback)) {
        progress_callback(70, "ExtractMplusresults...")
      }
      
      # Extract results
      mplus_results <- current_engine$extract_results(output_file)
      
      results$fit_indices <- mplus_results$fit_indices
      results$parameters <- mplus_results$parameters
      results$warnings <- mplus_results$warnings
      results$errors <- mplus_results$errors
      
    } else {
      # R/lavaan engine workflow
      if (!requireNamespace("lavaan", quietly = TRUE)) {
        stop("lavaan packageNot installed")
      }
      
      # Validate model syntax before running
      if (is.null(model_syntax) || length(model_syntax) == 0 || nchar(trimws(model_syntax)) == 0) {
        stop("Model Syntaxis emptyOr invalid")
      }
      
      # Run lavaan with error handling
      fit <- tryCatch({
        lavaan::sem(
          model = model_syntax,
          data = data,
          missing = "fiml",
          std.lv = TRUE,
          estimator = "MLR",
          warn = FALSE
        )
      }, error = function(e) {
        stop(paste("lavaanAnalysis failed:", e$message))
      })
      
      if (!is.null(progress_callback)) {
        progress_callback(60, "Extract fit indices...")
      }
      
      # Check if fit object is valid
      if (is.null(fit) || !inherits(fit, "lavaan")) {
        stop("lavaanFit object invalid")
      }
      
      # Extract fit indices with error handling
      fit_measures <- tryCatch({
        lavaan::fitMeasures(fit, c(
          "chisq", "df", "pvalue",
          "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
          "srmr", "aic", "bic"
        ))
      }, error = function(e) {
        stop(paste("Failed to extract fit indices:", e$message))
      })
      
      # Check if fit measures are valid
      if (is.null(fit_measures) || length(fit_measures) == 0) {
        stop("Fit indicesis empty")
      }
      
      # Fill any missing indices with NA to avoid zero-length errors downstream
      expected_keys <- c("chisq","df","pvalue","cfi","tli","rmsea",
                         "rmsea.ci.lower","rmsea.ci.upper","srmr","aic","bic")
      fm_list <- as.list(fit_measures)
      for (k in expected_keys) {
        if (is.null(fm_list[[k]]) || length(fm_list[[k]]) == 0) {
          fm_list[[k]] <- NA_real_
        }
      }
      results$fit_indices <- fm_list
      
      if (!is.null(progress_callback)) {
        progress_callback(80, "ExtractParameter Estimates...")
      }
      
      # Extract parameter estimates with error handling
      params <- tryCatch({
        lavaan::parameterEstimates(fit, standardized = TRUE)
      }, error = function(e) {
        NULL
      })
      results$parameters <- params
      
      # Check for warnings with safe evaluation
      converged <- tryCatch({
        lavaan::lavInspect(fit, "converged")
      }, error = function(e) {
        TRUE  # Assume converged if we can't check
      })
      
      if (!is.null(converged) && length(converged) > 0 && isFALSE(converged)) {
        results$warnings <- c(results$warnings, "Model did not converge")
      }
      
      results$fit_object <- fit
    }
    
    if (!is.null(progress_callback)) {
      progress_callback(100, "Analysis completed！")
    }
    
    results$success <- TRUE
    
  }, error = function(e) {
    results$errors <<- c(results$errors, as.character(e$message))
    if (!is.null(progress_callback)) {
      progress_callback(100, paste("Analysis failed:", e$message))
    }
  })
  
  results
}

#' Format fit indices for display
#' @param fit_indices List of fit indices
#' @return Data frame formatted for display
format_fit_indices <- function(fit_indices) {
  if (is.null(fit_indices)) {
    return(data.frame(
      Index = "No results",
      Value = "",
      Interpretation = ""
    ))
  }
  
  # Create interpretation
  interpret <- function(index, value) {
    switch(index,
      "cfi" = if (value >= 0.95) "Excellent" else if (value >= 0.90) "Acceptable" else "Poor",
      "tli" = if (value >= 0.95) "Excellent" else if (value >= 0.90) "Acceptable" else "Poor",
      "rmsea" = if (value <= 0.05) "Excellent" else if (value <= 0.08) "Acceptable" else "Poor",
      "srmr" = if (value <= 0.05) "Excellent" else if (value <= 0.08) "Acceptable" else "Poor",
      "pvalue" = if (value > 0.05) "Model fits well" else "Model fits poorly",
      ""
    )
  }
  
  # Safe numeric getter to avoid zero-length/NULL issues
  get_num <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_real_)
    suppressWarnings(as.numeric(x))
  }
  
  # Prepare safe values
  v_chisq <- get_num(fit_indices$chisq)
  v_df <- get_num(fit_indices$df)
  v_p <- get_num(fit_indices$pvalue)
  v_cfi <- get_num(fit_indices$cfi)
  v_tli <- get_num(fit_indices$tli)
  v_rmsea <- get_num(fit_indices$rmsea)
  v_rmsea_lo <- get_num(fit_indices$rmsea.ci.lower)
  v_rmsea_hi <- get_num(fit_indices$rmsea.ci.upper)
  v_srmr <- get_num(fit_indices$srmr)
  v_aic <- get_num(fit_indices$aic)
  v_bic <- get_num(fit_indices$bic)
  
  # Format values
  indices_df <- data.frame(
    Index = c("χ²", "df", "p-value", "CFI", "TLI", "RMSEA", 
              "RMSEA 90% CI", "SRMR", "AIC", "BIC"),
    Value = c(
      if (is.na(v_chisq)) "NA" else sprintf("%.2f", v_chisq),
      if (is.na(v_df)) "NA" else sprintf("%.0f", v_df),
      if (is.na(v_p)) "NA" else sprintf("%.3f", v_p),
      if (is.na(v_cfi)) "NA" else sprintf("%.3f", v_cfi),
      if (is.na(v_tli)) "NA" else sprintf("%.3f", v_tli),
      if (is.na(v_rmsea)) "NA" else sprintf("%.3f", v_rmsea),
      if (any(is.na(c(v_rmsea_lo, v_rmsea_hi)))) "[NA, NA]" else sprintf("[%.3f, %.3f]", v_rmsea_lo, v_rmsea_hi),
      if (is.na(v_srmr)) "NA" else sprintf("%.3f", v_srmr),
      if (is.na(v_aic)) "NA" else sprintf("%.2f", v_aic),
      if (is.na(v_bic)) "NA" else sprintf("%.2f", v_bic)
    ),
    Interpretation = c(
      "",
      "",
      interpret("pvalue", v_p),
      interpret("cfi", v_cfi),
      interpret("tli", v_tli),
      interpret("rmsea", v_rmsea),
      "",
      interpret("srmr", v_srmr),
      "",
      ""
    ),
    stringsAsFactors = FALSE
  )
  
  indices_df
}

#' Format parameter estimates for display
#' @param parameters Parameter estimates data frame
#' @return Formatted data frame
format_parameters <- function(parameters) {
  if (is.null(parameters) || nrow(parameters) == 0) {
    return(data.frame(
      Parameter = "No results",
      Estimate = "",
      SE = "",
      Z = "",
      P = "",
      Std = "",
      stringsAsFactors = FALSE
    ))
  }
  
  # Check if this is lavaan format (has lhs, op, rhs) or Mplus format
  if ("lhs" %in% names(parameters) && "op" %in% names(parameters) && "rhs" %in% names(parameters)) {
    # lavaan format
    params_df <- data.frame(
      From = parameters$lhs,
      Operator = parameters$op,
      To = parameters$rhs,
      Estimate = sprintf("%.3f", parameters$est),
      SE = sprintf("%.3f", parameters$se),
      Z = sprintf("%.3f", parameters$z),
      P = sprintf("%.3f", parameters$pvalue),
      Std = if ("std.all" %in% names(parameters)) sprintf("%.3f", parameters$std.all) else "",
      Sig = ifelse(parameters$pvalue < 0.001, "***",
                   ifelse(parameters$pvalue < 0.01, "**",
                          ifelse(parameters$pvalue < 0.05, "*", ""))),
      stringsAsFactors = FALSE
    )
  } else if ("paramHeader" %in% names(parameters) && "param" %in% names(parameters)) {
    # Mplus format
    params_df <- data.frame(
      Parameter = paste(parameters$paramHeader, parameters$param, sep = " "),
      Estimate = sprintf("%.3f", parameters$est),
      SE = sprintf("%.3f", parameters$se),
      Z = sprintf("%.3f", parameters$est / parameters$se),
      P = if ("pval" %in% names(parameters)) sprintf("%.3f", parameters$pval) else "",
      Std = if ("est_se" %in% names(parameters)) sprintf("%.3f", parameters$est_se) else "",
      Sig = if ("pval" %in% names(parameters)) {
        ifelse(parameters$pval < 0.001, "***",
               ifelse(parameters$pval < 0.01, "**",
                      ifelse(parameters$pval < 0.05, "*", "")))
      } else "",
      stringsAsFactors = FALSE
    )
  } else {
    # Unknown format, try to display what we have
    params_df <- data.frame(
      Parameter = if ("param" %in% names(parameters)) parameters$param else rownames(parameters),
      Estimate = if ("est" %in% names(parameters)) sprintf("%.3f", parameters$est) else "",
      SE = if ("se" %in% names(parameters)) sprintf("%.3f", parameters$se) else "",
      Z = "",
      P = "",
      Std = "",
      stringsAsFactors = FALSE
    )
  }
  
  params_df
}

#' Create progress tracker
#' @return List with progress tracking functions
create_progress_tracker <- function() {
  state <- list(
    progress = 0,
    message = "PreparingIn...",
    status = "pending"  # pending, running, complete, error
  )
  
  list(
    update = function(progress, message = NULL, status = NULL) {
      state$progress <<- progress
      if (!is.null(message)) state$message <<- message
      if (!is.null(status)) state$status <<- status
    },
    
    get = function() state,
    
    reset = function() {
      state$progress <<- 0
      state$message <<- "PreparingIn..."
      state$status <<- "pending"
    }
  )
}
