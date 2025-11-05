# Module Loader
# Centralized module loading and dependency management

#' Load all application modules
#' @param modules_dir Directory containing modules
#' @return List of loaded modules
load_modules <- function(modules_dir = "R/modules") {
  if (!exists(".loaded_modules", envir = .GlobalEnv)) {
    .GlobalEnv$.loaded_modules <- list()
  }
  
  # Define module loading order (dependencies first)
  module_order <- c(
    "config/app_config.R",
    "utils/logger.R",
    "utils/error_handler.R",
    "utils/helpers.R",
    "engines/engine_interface.R",
    "engines/mplus_engine.R",
    "engines/r_engine.R",
    "engines/engine_manager.R",  # Add engine manager
    "data/data_manager.R",
    "templates/template_engine.R",
    "batch/batch_processor.R",
    "results/results_analyzer.R",
    "visualization/plot_generator.R",
    "ui/ui_components.R"
  )
  
  loaded_count <- 0
  
  for (module_path in module_order) {
    full_path <- file.path("R", module_path)
    
    if (file.exists(full_path)) {
      tryCatch({
        source(full_path, local = FALSE)
        .GlobalEnv$.loaded_modules[[module_path]] <- TRUE
        loaded_count <- loaded_count + 1
      }, error = function(e) {
        warning(sprintf("Failed to load module %s: %s", module_path, e$message))
      })
    }
  }
  
  message(sprintf("Loaded %d modules successfully", loaded_count))
  invisible(.GlobalEnv$.loaded_modules)
}

#' Check if module is loaded
#' @param module_name Module name
#' @return Logical indicating if module is loaded
is_module_loaded <- function(module_name) {
  if (!exists(".loaded_modules", envir = .GlobalEnv)) {
    return(FALSE)
  }
  return(module_name %in% names(.GlobalEnv$.loaded_modules))
}

#' Reload specific module
#' @param module_path Path to module file
reload_module <- function(module_path) {
  full_path <- if (startsWith(module_path, "R/")) {
    module_path
  } else {
    file.path("R", module_path)
  }
  
  if (!file.exists(full_path)) {
    stop(sprintf("Module not found: %s", full_path))
  }
  
  tryCatch({
    source(full_path, local = FALSE)
    if (exists(".loaded_modules", envir = .GlobalEnv)) {
      .GlobalEnv$.loaded_modules[[module_path]] <- TRUE
    }
    message(sprintf("Reloaded module: %s", module_path))
    invisible(TRUE)
  }, error = function(e) {
    stop(sprintf("Failed to reload module %s: %s", module_path, e$message))
  })
}

#' Get list of loaded modules
#' @return Character vector of loaded module names
get_loaded_modules <- function() {
  if (!exists(".loaded_modules", envir = .GlobalEnv)) {
    return(character(0))
  }
  return(names(.GlobalEnv$.loaded_modules))
}

#' Initialize application
#' @param env Environment name
initialize_app <- function(env = "development") {
  message("Initializing SEM Batch Processor...")
  
  # Load configuration
  if (file.exists("R/config/app_config.R")) {
    source("R/config/app_config.R")
    initialize_config(env)
  }
  
  # Initialize logger
  if (file.exists("R/utils/logger.R")) {
    source("R/utils/logger.R")
    
    # Get logging configuration
    log_enabled <- get_config("logging.file", TRUE)
    log_level <- get_config("logging.level", "INFO")
    log_console <- get_config("logging.console", TRUE)
    
    # Construct log file path
    log_file <- if (is.logical(log_enabled) && log_enabled) {
      log_dir <- get_config("logging.file_path", "logs")
      if (!dir.exists(log_dir)) {
        dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
      }
      file.path(log_dir, paste0("app_", Sys.Date(), ".log"))
    } else if (is.character(log_enabled)) {
      log_enabled
    } else {
      NULL
    }
    
    initialize_logger(log_file, log_level, log_console)
    log_info("Application initialization started")
  }
  
  # Load all modules
  load_modules()
  
  # Clean old checkpoints
  if (exists("clean_checkpoints", mode = "function")) {
    clean_checkpoints()
  }
  
  if (exists("log_info", mode = "function")) {
    log_info("Application initialization completed")
  }
  
  message("SEM Batch Processor initialized successfully!")
  invisible(TRUE)
}
