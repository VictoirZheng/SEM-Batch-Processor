# SEM Batch Processor - Shiny entry point
#
# Run locally:  shiny::runApp()        (from the project folder)
# Deploy:       rsconnect::deployApp() (online mode is detected automatically)

if (.Platform$OS.type != "windows" && !isTRUE(l10n_info()$`UTF-8`)) {
  try(Sys.setlocale("LC_CTYPE", "C.UTF-8"), silent = TRUE)
}

local({
  required <- c("shiny", "shinydashboard", "DT", "lavaan", "callr", "processx", "openxlsx", "yaml", "zip")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing R packages: ", paste(missing, collapse = ", "),
         "\nInstall them with:\ninstall.packages(c(", paste0('"', missing, '"', collapse = ", "), "))",
         call. = FALSE)
  }
})

library(shiny)

for (f in c(sort(list.files("R/core", pattern = "\\.R$", full.names = TRUE)),
            sort(list.files("R/app", pattern = "\\.R$", full.names = TRUE)))) {
  source(f, encoding = "UTF-8", local = globalenv())
}

# Defined in the global environment because the UI and server functions
# (sourced above into the global environment) refer to it.
assign("APP_CONFIG", load_app_config(), envir = globalenv())
options(shiny.maxRequestSize = APP_CONFIG$limit$max_upload_mb * 1024^2)

shinyApp(ui = app_ui, server = app_server)
