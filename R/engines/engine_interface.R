# Analysis Engine Interface
# Unified interface for SEM analysis engines (Mplus and R/lavaan)

#' Create analysis engine interface
#' @description Defines the standard interface that all analysis engines must implement
#' @return List of interface methods
create_engine_interface <- function() {
  interface <- list(
    # Engine identification
    name = NULL,
    version = NULL,
    type = NULL,  # "mplus" or "r"
    
    # Availability check
    is_available = function() {
      stop("Method 'is_available' must be implemented by engine")
    },
    
    # Engine initialization
    initialize = function(config = NULL) {
      stop("Method 'initialize' must be implemented by engine")
    },
    
    # Model specification
    parse_model = function(model_spec) {
      stop("Method 'parse_model' must be implemented by engine")
    },
    
    validate_model = function(model_spec) {
      stop("Method 'validate_model' must be implemented by engine")
    },
    
    # Data handling
    prepare_data = function(data, data_path = NULL) {
      stop("Method 'prepare_data' must be implemented by engine")
    },
    
    # Model execution
    run_model = function(model_spec, data_path, output_path = NULL) {
      stop("Method 'run_model' must be implemented by engine")
    },
    
    run_batch = function(model_specs, data_path, output_dir = NULL) {
      stop("Method 'run_batch' must be implemented by engine")
    },
    
    # Results extraction
    extract_results = function(output_path) {
      stop("Method 'extract_results' must be implemented by engine")
    },
    
    extract_fit_indices = function(output_path) {
      stop("Method 'extract_fit_indices' must be implemented by engine")
    },
    
    extract_parameters = function(output_path) {
      stop("Method 'extract_parameters' must be implemented by engine")
    },
    
    # Visualization
    generate_diagram = function(model_spec, output_path = NULL) {
      stop("Method 'generate_diagram' must be implemented by engine")
    },
    
    # Cleanup
    cleanup = function() {
      stop("Method 'cleanup' must be implemented by engine")
    }
  )
  
  class(interface) <- c("AnalysisEngine", "list")
  return(interface)
}

#' Engine registry for managing available engines
.engine_registry <- new.env(parent = emptyenv())

#' Register an analysis engine
#' @param engine_name Engine name
#' @param engine_constructor Function that creates the engine
register_engine <- function(engine_name, engine_constructor) {
  if (!is.function(engine_constructor)) {
    stop("engine_constructor must be a function")
  }
  
  .engine_registry[[engine_name]] <- engine_constructor
  
  if (exists("log_info", mode = "function")) {
    log_info(sprintf("Engine registered: %s", engine_name))
  }
  
  invisible(TRUE)
}

#' Get registered engine
#' @param engine_name Engine name
#' @return Engine constructor function
get_engine_constructor <- function(engine_name) {
  if (!engine_name %in% names(.engine_registry)) {
    stop(sprintf("Engine not registered: %s", engine_name))
  }
  
  return(.engine_registry[[engine_name]])
}

#' List registered engines
#' @return Character vector of registered engine names
list_registered_engines <- function() {
  return(names(.engine_registry))
}

#' Create engine instance
#' @param engine_name Engine name ("mplus" or "r")
#' @param config Engine configuration
#' @return Engine instance
create_engine <- function(engine_name, config = NULL) {
  if (!engine_name %in% names(.engine_registry)) {
    stop(sprintf("Engine not registered: %s. Available engines: %s",
                 engine_name, paste(names(.engine_registry), collapse = ", ")))
  }
  
  constructor <- .engine_registry[[engine_name]]
  engine <- constructor(config)
  
  if (exists("log_info", mode = "function")) {
    log_info(sprintf("Engine created: %s", engine_name))
  }
  
  return(engine)
}

#' Detect available engines
#' @return List of available engines with their status
detect_available_engines <- function() {
  engines <- list()
  
  # Check Mplus
  mplus_available <- FALSE
  mplus_version <- NULL
  
  tryCatch({
    if (requireNamespace("MplusAutomation", quietly = TRUE)) {
      # Try to detect Mplus installation
      mplus_path <- Sys.which("mplus")
      if (mplus_path != "") {
        mplus_available <- TRUE
        # Try to get version
        version_output <- system("mplus -v", intern = TRUE, ignore.stderr = TRUE)
        if (length(version_output) > 0) {
          mplus_version <- version_output[1]
        }
      }
    }
  }, error = function(e) {
    mplus_available <- FALSE
  })
  
  engines$mplus <- list(
    available = mplus_available,
    version = mplus_version,
    package_installed = requireNamespace("MplusAutomation", quietly = TRUE)
  )
  
  # Check R/lavaan
  r_available <- FALSE
  lavaan_version <- NULL
  
  tryCatch({
    if (requireNamespace("lavaan", quietly = TRUE)) {
      r_available <- TRUE
      lavaan_version <- as.character(packageVersion("lavaan"))
    }
  }, error = function(e) {
    r_available <- FALSE
  })
  
  engines$r <- list(
    available = r_available,
    version = lavaan_version,
    package_installed = requireNamespace("lavaan", quietly = TRUE),
    semplot_installed = requireNamespace("semPlot", quietly = TRUE)
  )
  
  return(engines)
}

#' Get recommended engine
#' @param preference User preference ("auto", "mplus", or "r")
#' @return Recommended engine name
get_recommended_engine <- function(preference = "auto") {
  available_engines <- detect_available_engines()
  
  if (preference != "auto") {
    # Check if preferred engine is available
    if (preference %in% names(available_engines) && 
        available_engines[[preference]]$available) {
      return(preference)
    } else {
      if (exists("log_warning", mode = "function")) {
        log_warning(sprintf("Preferred engine '%s' not available, using auto-detection", 
                           preference))
      }
    }
  }
  
  # Auto-detect best available engine (prefer R by default)
  if (available_engines$r$available) {
    return("r")
  } else if (available_engines$mplus$available) {
    return("mplus")
  } else {
    stop("No analysis engines available. Please install lavaan package (R) or Mplus.")
  }
}

#' Engine factory - creates appropriate engine based on preference
#' @param preference Engine preference ("auto", "mplus", or "r")
#' @param config Engine configuration
#' @return Engine instance
engine_factory <- function(preference = "auto", config = NULL) {
  # Get configuration if not provided
  if (is.null(config)) {
    if (exists("get_config", mode = "function")) {
      config <- get_config("engines")
    }
  }
  
  # Determine which engine to use
  engine_name <- if (preference == "auto") {
    pref <- if (!is.null(config$default)) config$default else "auto"
    get_recommended_engine(pref)
  } else {
    preference
  }
  
  # Create and initialize engine
  engine <- create_engine(engine_name, config)
  
  # Check if engine is available
  if (!engine$is_available()) {
    stop(sprintf("Engine '%s' is not available on this system", engine_name))
  }
  
  # Initialize engine
  engine$initialize(config)
  
  if (exists("log_info", mode = "function")) {
    log_info(sprintf("Engine initialized: %s (version: %s)", 
                    engine$name, engine$version))
  }
  
  return(engine)
}

#' Standardize results format across engines
#' @param results Raw results from engine
#' @param engine_type Engine type ("mplus" or "r")
#' @return Standardized results list
standardize_results <- function(results, engine_type) {
  standard_results <- list(
    engine = engine_type,
    timestamp = Sys.time(),
    
    # Model fit
    fit_indices = list(
      chisq = NA,
      df = NA,
      pvalue = NA,
      cfi = NA,
      tli = NA,
      rmsea = NA,
      rmsea_ci_lower = NA,
      rmsea_ci_upper = NA,
      srmr = NA,
      aic = NA,
      bic = NA
    ),
    
    # Parameters
    parameters = data.frame(
      label = character(),
      estimate = numeric(),
      se = numeric(),
      z_value = numeric(),
      pvalue = numeric(),
      ci_lower = numeric(),
      ci_upper = numeric(),
      std_estimate = numeric(),
      stringsAsFactors = FALSE
    ),
    
    # Additional info
    convergence = NA,
    warnings = character(),
    errors = character(),
    
    # Raw results for reference
    raw_results = results
  )
  
  class(standard_results) <- c("StandardizedSEMResults", "list")
  return(standard_results)
}

#' Validate engine implementation
#' @param engine Engine instance
#' @return List with validation results
validate_engine_implementation <- function(engine) {
  required_methods <- c(
    "is_available", "initialize", "parse_model", "validate_model",
    "prepare_data", "run_model", "run_batch", "extract_results",
    "extract_fit_indices", "extract_parameters", "generate_diagram", "cleanup"
  )
  
  missing_methods <- character()
  
  for (method in required_methods) {
    if (!method %in% names(engine) || !is.function(engine[[method]])) {
      missing_methods <- c(missing_methods, method)
    }
  }
  
  valid <- length(missing_methods) == 0
  
  return(list(
    valid = valid,
    missing_methods = missing_methods,
    implemented_methods = setdiff(required_methods, missing_methods)
  ))
}

#' Get engine status information
#' @param engine Engine instance
#' @return List of status information
get_engine_status <- function(engine) {
  status <- list(
    name = engine$name,
    type = engine$type,
    version = engine$version,
    available = engine$is_available(),
    validation = validate_engine_implementation(engine)
  )
  
  return(status)
}

#' Compare engines capabilities
#' @return Data frame comparing engine features
compare_engines <- function() {
  available <- detect_available_engines()
  
  comparison <- data.frame(
    Feature = c(
      "Available",
      "Package Installed",
      "Version",
      "Bootstrap Support",
      "Missing Data (FIML)",
      "Categorical Variables",
      "Multilevel Models",
      "Mixture Models",
      "Bayesian Estimation",
      "Diagram Generation"
    ),
    Mplus = c(
      available$mplus$available,
      available$mplus$package_installed,
      ifelse(is.null(available$mplus$version), "N/A", available$mplus$version),
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      "Yes",
      "Limited"
    ),
    R_lavaan = c(
      available$r$available,
      available$r$package_installed,
      ifelse(is.null(available$r$version), "N/A", available$r$version),
      "Yes",
      "Yes",
      "Yes (with estimator)",
      "Limited",
      "No",
      "Via blavaan",
      "Yes (semPlot)"
    ),
    stringsAsFactors = FALSE
  )
  
  return(comparison)
}
