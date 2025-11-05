# SEM Batch Processor - Main Application Entry Point
# Version 2.0.0
# Enhanced R Shiny application for batch SEM analysis

# Load required packages
required_packages <- c(
  "shiny",
  "shinydashboard",
  "shinyjs",
  "shinyWidgets",
  "DT",
  "plotly",
  "ggplot2",
  "dplyr",
  "tidyr",
  "stringr",
  "lavaan",
  "psych",
  "progress",
  "crayon"
)

# Optional packages (will use if available)
optional_packages <- c(
  "yaml",
  "MplusAutomation",
  "semPlot",
  "foreign",
  "readxl",
  "openxlsx",
  "future",
  "promises",
  "shinyAce"  # For advanced code editor
)

# Check and install missing required packages
missing_packages <- required_packages[!sapply(required_packages, function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
})]

if (length(missing_packages) > 0) {
  message(sprintf("Installing %d missing required packages...", length(missing_packages)))
  message("Missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, dependencies = TRUE)
}

# Load required packages
suppressPackageStartupMessages({
  for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      library(pkg, character.only = TRUE, quietly = TRUE)
    } else {
      warning(sprintf("Failed to load required package: %s", pkg))
    }
  }
})

# Try to load optional packages
for (pkg in optional_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    suppressPackageStartupMessages({
      library(pkg, character.only = TRUE, quietly = TRUE)
    })
  }
}

# Source module loader
source("R/modules/module_loader.R")

# Initialize application
initialize_app(env = Sys.getenv("APP_ENV", "development"))

# Source UI and Server components
if (file.exists("R/ui/ui_main.R")) {
  source("R/ui/ui_main.R")
  log_info("UI loaded successfully")
}

if (file.exists("R/server/server_main.R")) {
  source("R/server/server_main.R")
  log_info("Server loaded successfully")
}

# Run the application
shinyApp(ui = ui, server = server)
