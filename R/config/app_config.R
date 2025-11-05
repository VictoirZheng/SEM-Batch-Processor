# Application Configuration Management
# This module handles all configuration settings for the SEM Batch Processor

#' Load YAML configuration file
#' @param file_path Path to YAML file
#' @return List of configuration settings
load_yaml_config <- function(file_path) {
  if (!file.exists(file_path)) {
    return(list())
  }
  
  tryCatch({
    if (requireNamespace("yaml", quietly = TRUE)) {
      return(yaml::yaml.load_file(file_path))
    } else {
      warning("yaml package not installed, using default configuration")
      return(list())
    }
  }, error = function(e) {
    warning(sprintf("Failed to load config file %s: %s", file_path, e$message))
    return(list())
  })
}

#' Merge configuration lists recursively
#' @param base Base configuration
#' @param override Override configuration
#' @return Merged configuration
merge_config <- function(base, override) {
  if (!is.list(base) || !is.list(override)) {
    return(override)
  }
  
  result <- base
  
  for (name in names(override)) {
    if (name %in% names(base) && is.list(base[[name]]) && is.list(override[[name]])) {
      result[[name]] <- merge_config(base[[name]], override[[name]])
    } else {
      result[[name]] <- override[[name]]
    }
  }
  
  return(result)
}

#' Get default configuration
#' @return Default configuration list
get_default_config <- function() {
  list(
    app = list(
      name = "SEMBatch ProcessingProcessor",
      version = "2.0.0",
      language = "zh-CN"
    ),
    ui = list(
      theme = "flatly",
      max_file_size_mb = 100,
      items_per_page = 20
    ),
    engines = list(
      default = "auto",
      mplus = list(enabled = TRUE, timeout_seconds = 3600),
      r = list(enabled = TRUE, default_estimator = "ML")
    ),
    processing = list(
      max_parallel_jobs = 4,
      checkpoint_interval = 10,
      auto_save = TRUE
    ),
    logging = list(
      level = "INFO",
      console = TRUE,
      file = TRUE,  # Enable file logging
      file_path = "logs",  # Directory for log files
      max_file_size_mb = 10,
      max_files = 5
    ),
    paths = list(
      temp = tempdir(),
      data = "data",
      output = "output",
      templates = "templates",
      logs = "logs"
    )
  )
}

#' Load application configuration
#' @param env Environment name ("development" or "production")
#' @return List of configuration settings
load_config <- function(env = "development") {
  # Start with default configuration
  config <- get_default_config()
  
  # Try to load YAML configuration if yaml package is available
  if (requireNamespace("yaml", quietly = TRUE)) {
    # Load default configuration from YAML
    yaml_config <- load_yaml_config("config/default.yaml")
    if (length(yaml_config) > 0) {
      config <- merge_config(config, yaml_config)
    }
    
    # Load user preferences
    user_config <- load_yaml_config("config/user_preferences.yaml")
    if (length(user_config) > 0) {
      config <- merge_config(config, user_config)
    }
    
    # Load environment-specific config if exists
    env_config_file <- sprintf("config/%s.yaml", env)
    env_config <- load_yaml_config(env_config_file)
    if (length(env_config) > 0) {
      config <- merge_config(config, env_config)
    }
  } else {
    message("Note: yaml package not installed. Using default configuration.")
    message("Install yaml package for custom configuration support: install.packages('yaml')")
  }
  
  # Add runtime information
  config$environment <- env
  config$loaded_at <- Sys.time()
  
  # Ensure paths exist
  if (!is.null(config$paths)) {
    for (path_name in names(config$paths)) {
      path <- config$paths[[path_name]]
      if (!is.null(path) && !dir.exists(path)) {
        dir.create(path, recursive = TRUE, showWarnings = FALSE)
      }
    }
  }
  
  return(config)
}

#' Validate configuration
#' @param config Configuration list
#' @return List with valid flag and error messages
validate_config <- function(config) {
  errors <- character()
  warnings <- character()
  
  # Check required fields
  required_fields <- c("app", "engines", "processing")
  for (field in required_fields) {
    if (!field %in% names(config)) {
      errors <- c(errors, sprintf("Missing required field: %s", field))
    }
  }
  
  # Validate numeric ranges
  if (!is.null(config$processing$max_parallel_jobs)) {
    if (config$processing$max_parallel_jobs < 1 || config$processing$max_parallel_jobs > 32) {
      warnings <- c(warnings, "max_parallel_jobs should be between 1 and 32")
    }
  }
  
  if (!is.null(config$ui$max_file_size_mb)) {
    if (config$ui$max_file_size_mb < 1 || config$ui$max_file_size_mb > 1000) {
      warnings <- c(warnings, "max_file_size_mb should be between 1 and 1000")
    }
  }
  
  # Validate engine settings
  if (!is.null(config$engines$default)) {
    valid_engines <- c("auto", "mplus", "r")
    if (!config$engines$default %in% valid_engines) {
      errors <- c(errors, sprintf("Invalid default engine: %s", config$engines$default))
    }
  }
  
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  ))
}

#' Get configuration value
#' @param key Configuration key (supports nested keys with dots)
#' @param default Default value if key not found
#' @return Configuration value
get_config <- function(key, default = NULL) {
  if (!exists(".app_config", envir = .GlobalEnv)) {
    .GlobalEnv$.app_config <- load_config()
  }
  
  keys <- strsplit(key, "\\.")[[1]]
  value <- .GlobalEnv$.app_config
  
  for (k in keys) {
    if (is.list(value) && k %in% names(value)) {
      value <- value[[k]]
    } else {
      return(default)
    }
  }
  
  return(value)
}

#' Set configuration value
#' @param key Configuration key
#' @param value New value
set_config <- function(key, value) {
  if (!exists(".app_config", envir = .GlobalEnv)) {
    .GlobalEnv$.app_config <- load_config()
  }
  
  keys <- strsplit(key, "\\.")[[1]]
  config <- .GlobalEnv$.app_config
  
  # Navigate to the parent of the target key
  current <- config
  for (i in seq_along(keys)[-length(keys)]) {
    if (!is.list(current[[keys[i]]])) {
      current[[keys[i]]] <- list()
    }
    current <- current[[keys[i]]]
  }
  
  # Set the value
  current[[keys[length(keys)]]] <- value
  .GlobalEnv$.app_config <- config
  
  invisible(TRUE)
}

#' Initialize application configuration
#' @param env Environment name
initialize_config <- function(env = Sys.getenv("APP_ENV", "development")) {
  config <- load_config(env)
  .GlobalEnv$.app_config <- config
  
  # Create necessary directories
  for (path_name in names(config$paths)) {
    path <- config$paths[[path_name]]
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  message(sprintf("Configuration initialized for %s environment", env))
  invisible(config)
}

#' Save user preferences
#' @param preferences List of preferences to save
#' @return TRUE if successful
save_user_preferences <- function(preferences) {
  pref_file <- "config/user_preferences.yaml"
  
  # Load existing preferences
  existing_prefs <- load_yaml_config(pref_file)
  
  # Merge with new preferences
  updated_prefs <- merge_config(existing_prefs, preferences)
  
  # Save to file
  tryCatch({
    if (requireNamespace("yaml", quietly = TRUE)) {
      yaml_content <- yaml::as.yaml(updated_prefs)
      writeLines(yaml_content, pref_file)
      
      if (exists("log_info", mode = "function")) {
        log_info("User preferences saved successfully")
      }
      return(TRUE)
    } else {
      warning("yaml package not installed, cannot save preferences")
      return(FALSE)
    }
  }, error = function(e) {
    if (exists("log_error", mode = "function")) {
      log_error(sprintf("Failed to save preferences: %s", e$message))
    }
    return(FALSE)
  })
}

#' Get user preference
#' @param key Preference key (supports nested keys with dots)
#' @param default Default value if not found
#' @return Preference value
get_user_preference <- function(key, default = NULL) {
  prefs <- load_yaml_config("config/user_preferences.yaml")
  
  keys <- strsplit(key, "\\.")[[1]]
  value <- prefs
  
  for (k in keys) {
    if (is.list(value) && k %in% names(value)) {
      value <- value[[k]]
    } else {
      return(default)
    }
  }
  
  return(value)
}

#' Set user preference
#' @param key Preference key (supports nested keys with dots)
#' @param value Preference value
#' @return TRUE if successful
set_user_preference <- function(key, value) {
  keys <- strsplit(key, "\\.")[[1]]
  
  # Build nested list structure
  pref_update <- list()
  current <- pref_update
  
  for (i in seq_along(keys)) {
    if (i == length(keys)) {
      current[[keys[i]]] <- value
    } else {
      current[[keys[i]]] <- list()
      current <- current[[keys[i]]]
    }
  }
  
  return(save_user_preferences(pref_update))
}

#' Reset user preferences to defaults
#' @return TRUE if successful
reset_user_preferences <- function() {
  pref_file <- "config/user_preferences.yaml"
  
  tryCatch({
    # Create empty preferences file
    writeLines("# User Preferences Configuration\n# Reset to defaults\n", pref_file)
    
    if (exists("log_info", mode = "function")) {
      log_info("User preferences reset to defaults")
    }
    
    # Reload configuration
    if (exists(".app_config", envir = .GlobalEnv)) {
      .GlobalEnv$.app_config <- load_config()
    }
    
    return(TRUE)
  }, error = function(e) {
    if (exists("log_error", mode = "function")) {
      log_error(sprintf("Failed to reset preferences: %s", e$message))
    }
    return(FALSE)
  })
}

#' Export configuration to file
#' @param file_path Output file path
#' @param include_defaults Whether to include default values
#' @return TRUE if successful
export_config <- function(file_path, include_defaults = TRUE) {
  config <- if (exists(".app_config", envir = .GlobalEnv)) {
    .GlobalEnv$.app_config
  } else {
    load_config()
  }
  
  # Remove runtime information
  config$loaded_at <- NULL
  config$environment <- NULL
  
  tryCatch({
    if (requireNamespace("yaml", quietly = TRUE)) {
      yaml_content <- yaml::as.yaml(config)
      writeLines(yaml_content, file_path)
      
      if (exists("log_info", mode = "function")) {
        log_info("Configuration exported", file = file_path)
      }
      return(TRUE)
    } else {
      # Fallback to JSON if yaml not available
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        json_content <- jsonlite::toJSON(config, pretty = TRUE, auto_unbox = TRUE)
        writeLines(json_content, file_path)
        return(TRUE)
      } else {
        warning("Neither yaml nor jsonlite package available")
        return(FALSE)
      }
    }
  }, error = function(e) {
    if (exists("log_error", mode = "function")) {
      log_error(sprintf("Failed to export configuration: %s", e$message))
    }
    return(FALSE)
  })
}

#' Import configuration from file
#' @param file_path Input file path
#' @param merge_with_existing Whether to merge with existing config
#' @return TRUE if successful
import_config <- function(file_path, merge_with_existing = TRUE) {
  if (!file.exists(file_path)) {
    stop(sprintf("Configuration file not found: %s", file_path))
  }
  
  imported_config <- tryCatch({
    if (grepl("\\.ya?ml$", file_path, ignore.case = TRUE)) {
      load_yaml_config(file_path)
    } else if (grepl("\\.json$", file_path, ignore.case = TRUE)) {
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        jsonlite::fromJSON(file_path)
      } else {
        stop("jsonlite package required for JSON import")
      }
    } else {
      stop("Unsupported file format. Use .yaml or .json")
    }
  }, error = function(e) {
    stop(sprintf("Failed to import configuration: %s", e$message))
  })
  
  # Validate imported configuration
  validation <- validate_config(imported_config)
  if (!validation$valid) {
    stop(sprintf("Invalid configuration: %s", paste(validation$errors, collapse = "; ")))
  }
  
  if (length(validation$warnings) > 0 && exists("log_warning", mode = "function")) {
    for (warn in validation$warnings) {
      log_warning(warn)
    }
  }
  
  # Save as user preferences
  if (merge_with_existing) {
    return(save_user_preferences(imported_config))
  } else {
    # Replace entire user preferences
    tryCatch({
      if (requireNamespace("yaml", quietly = TRUE)) {
        yaml_content <- yaml::as.yaml(imported_config)
        writeLines(yaml_content, "config/user_preferences.yaml")
        
        # Reload configuration
        if (exists(".app_config", envir = .GlobalEnv)) {
          .GlobalEnv$.app_config <- load_config()
        }
        
        if (exists("log_info", mode = "function")) {
          log_info("Configuration imported successfully")
        }
        return(TRUE)
      }
    }, error = function(e) {
      if (exists("log_error", mode = "function")) {
        log_error(sprintf("Failed to save imported configuration: %s", e$message))
      }
      return(FALSE)
    })
  }
}

#' Initialize application configuration
#' @param env Environment name
initialize_config <- function(env = Sys.getenv("APP_ENV", "development")) {
  config <- load_config(env)
  
  # Validate configuration
  validation <- validate_config(config)
  
  if (!validation$valid) {
    warning(sprintf("Configuration validation failed: %s", 
                    paste(validation$errors, collapse = "; ")))
    warning("Using default configuration as fallback")
    config <- get_default_config()
    config$environment <- env
    config$loaded_at <- Sys.time()
  }
  
  if (length(validation$warnings) > 0) {
    for (warn in validation$warnings) {
      warning(warn)
    }
  }
  
  .GlobalEnv$.app_config <- config
  
  # Create necessary directories
  paths_to_create <- c("logs", "data", "output", "templates")
  for (path in paths_to_create) {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  # Create logging directory if specified
  if (!is.null(config$logging$file_path)) {
    if (!dir.exists(config$logging$file_path)) {
      dir.create(config$logging$file_path, recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  # Create templates directory if specified
  if (!is.null(config$templates) && !is.null(config$templates$library_path)) {
    if (!dir.exists(config$templates$library_path)) {
      dir.create(config$templates$library_path, recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  message(sprintf("Configuration initialized for %s environment", env))
  invisible(config)
}
