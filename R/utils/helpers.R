# Helper Utility Functions
# Common utility functions used across the application

#' Check if package is installed
#' @param package_name Package name
#' @return Logical indicating if package is installed
is_package_installed <- function(package_name) {
  return(requireNamespace(package_name, quietly = TRUE))
}

#' Install missing packages
#' @param packages Vector of package names
#' @param repos CRAN repository
install_missing_packages <- function(packages, repos = "https://cloud.r-project.org") {
  missing_packages <- packages[!sapply(packages, is_package_installed)]
  
  if (length(missing_packages) > 0) {
    message(sprintf("Installing %d missing packages...", length(missing_packages)))
    install.packages(missing_packages, repos = repos, dependencies = TRUE)
    return(missing_packages)
  }
  
  return(character(0))
}

#' Load required packages
#' @param packages Vector of package names
#' @param install_if_missing Whether to install missing packages
load_packages <- function(packages, install_if_missing = TRUE) {
  if (install_if_missing) {
    install_missing_packages(packages)
  }
  
  loaded <- sapply(packages, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE))
  })
  
  if (all(loaded)) {
    if (exists("log_info", mode = "function")) {
      log_info(sprintf("Loaded %d packages", length(packages)))
    }
  } else {
    failed <- packages[!loaded]
    if (exists("log_warning", mode = "function")) {
      log_warning(sprintf("Failed to load packages: %s", paste(failed, collapse = ", ")))
    }
  }
  
  invisible(loaded)
}

#' Format number with specified decimal places
#' @param x Numeric value
#' @param digits Number of decimal places
#' @return Formatted string
format_number <- function(x, digits = 3) {
  if (is.na(x) || is.null(x)) {
    return("NA")
  }
  return(sprintf(paste0("%.", digits, "f"), x))
}

#' Format p-value
#' @param p P-value
#' @return Formatted p-value string
format_pvalue <- function(p) {
  if (is.na(p) || is.null(p)) {
    return("NA")
  }
  if (p < 0.001) {
    return("<.001")
  }
  return(format_number(p, 3))
}

#' Create progress bar
#' @param total Total number of iterations
#' @param title Progress bar title
#' @return Progress bar object
create_progress_bar <- function(total, title = "Processing") {
  if (is_package_installed("progress")) {
    pb <- progress::progress_bar$new(
      format = paste0(title, " [:bar] :percent :eta"),
      total = total,
      clear = FALSE,
      width = 60
    )
    return(pb)
  }
  return(NULL)
}

#' Update progress bar
#' @param pb Progress bar object
#' @param step Step size
update_progress <- function(pb, step = 1) {
  if (!is.null(pb) && inherits(pb, "progress_bar")) {
    pb$tick(step)
  }
}

#' Generate unique ID
#' @param prefix ID prefix
#' @return Unique identifier string
generate_id <- function(prefix = "id") {
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  random <- sample(1000:9999, 1)
  return(paste0(prefix, "_", timestamp, "_", random))
}

#' Clean temporary files
#' @param pattern File pattern to match
#' @param max_age_hours Maximum age in hours
clean_temp_files <- function(pattern = NULL, max_age_hours = 24) {
  temp_dir <- tempdir()
  
  files <- if (is.null(pattern)) {
    list.files(temp_dir, full.names = TRUE, recursive = TRUE)
  } else {
    list.files(temp_dir, pattern = pattern, full.names = TRUE, recursive = TRUE)
  }
  
  current_time <- Sys.time()
  removed_count <- 0
  
  for (file in files) {
    if (file.exists(file)) {
      file_age <- difftime(current_time, file.info(file)$mtime, units = "hours")
      if (file_age > max_age_hours) {
        tryCatch({
          file.remove(file)
          removed_count <- removed_count + 1
        }, error = function(e) {
          # Silently ignore errors
        })
      }
    }
  }
  
  if (removed_count > 0 && exists("log_info", mode = "function")) {
    log_info(sprintf("Cleaned %d temporary files", removed_count))
  }
  
  invisible(removed_count)
}

#' Convert list to data frame safely
#' @param lst List to convert
#' @return Data frame
list_to_dataframe <- function(lst) {
  if (length(lst) == 0) {
    return(data.frame())
  }
  
  # Ensure all elements have the same length
  max_length <- max(sapply(lst, length))
  lst_padded <- lapply(lst, function(x) {
    if (length(x) < max_length) {
      c(x, rep(NA, max_length - length(x)))
    } else {
      x
    }
  })
  
  tryCatch({
    as.data.frame(lst_padded, stringsAsFactors = FALSE)
  }, error = function(e) {
    if (exists("log_warning", mode = "function")) {
      log_warning(sprintf("Failed to convert list to data frame: %s", e$message))
    }
    return(data.frame())
  })
}

#' Read file with encoding detection
#' @param file_path Path to file
#' @return File contents as character vector
read_file_safe <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(sprintf("File not found: %s", file_path))
  }
  
  # Try different encodings
  encodings <- c("UTF-8", "latin1", "CP1252")
  
  for (enc in encodings) {
    content <- tryCatch({
      readLines(file_path, encoding = enc, warn = FALSE)
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(content)) {
      return(content)
    }
  }
  
  stop(sprintf("Failed to read file with any encoding: %s", file_path))
}

#' Write file safely with backup
#' @param content Content to write
#' @param file_path Path to file
#' @param backup Whether to create backup
write_file_safe <- function(content, file_path, backup = TRUE) {
  # Create backup if file exists
  if (backup && file.exists(file_path)) {
    backup_path <- paste0(file_path, ".backup")
    file.copy(file_path, backup_path, overwrite = TRUE)
  }
  
  # Ensure directory exists
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Write content
  tryCatch({
    if (is.character(content)) {
      writeLines(content, file_path)
    } else {
      write(content, file_path)
    }
    return(TRUE)
  }, error = function(e) {
    if (exists("log_error", mode = "function")) {
      log_error(sprintf("Failed to write file: %s", e$message))
    }
    
    # Restore backup if write failed
    if (backup && file.exists(paste0(file_path, ".backup"))) {
      file.copy(paste0(file_path, ".backup"), file_path, overwrite = TRUE)
    }
    
    stop(e)
  })
}

#' Get file size in human-readable format
#' @param file_path Path to file
#' @return Formatted file size string
get_file_size <- function(file_path) {
  if (!file.exists(file_path)) {
    return("File not found")
  }
  
  size_bytes <- file.info(file_path)$size
  
  if (size_bytes < 1024) {
    return(sprintf("%d B", size_bytes))
  } else if (size_bytes < 1024^2) {
    return(sprintf("%.2f KB", size_bytes / 1024))
  } else if (size_bytes < 1024^3) {
    return(sprintf("%.2f MB", size_bytes / 1024^2))
  } else {
    return(sprintf("%.2f GB", size_bytes / 1024^3))
  }
}

#' Check system resources
#' @return List of system resource information
check_system_resources <- function() {
  resources <- list(
    memory_total_mb = NA,
    memory_available_mb = NA,
    cpu_cores = parallel::detectCores(),
    temp_dir = tempdir(),
    temp_space_gb = NA
  )
  
  # Try to get memory information (platform-specific)
  tryCatch({
    if (.Platform$OS.type == "windows") {
      # Windows-specific memory check
      mem_info <- system("wmic OS get FreePhysicalMemory,TotalVisibleMemorySize /Value", intern = TRUE)
      resources$memory_total_mb <- as.numeric(gsub(".*=", "", grep("TotalVisibleMemorySize", mem_info, value = TRUE))) / 1024
      resources$memory_available_mb <- as.numeric(gsub(".*=", "", grep("FreePhysicalMemory", mem_info, value = TRUE))) / 1024
    }
  }, error = function(e) {
    # Silently ignore if memory info cannot be retrieved
  })
  
  # Check temp directory space
  tryCatch({
    temp_info <- file.info(tempdir())
    if (!is.na(temp_info$size)) {
      resources$temp_space_gb <- temp_info$size / (1024^3)
    }
  }, error = function(e) {
    # Silently ignore
  })
  
  return(resources)
}
