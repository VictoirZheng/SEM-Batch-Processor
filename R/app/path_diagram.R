# Path diagram of a fitted lavaan model (standardized estimates).

draw_path_diagram <- function(fit, title = "") {
  if (!requireNamespace("semPlot", quietly = TRUE)) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "Install the 'semPlot' package to draw path diagrams:\ninstall.packages(\"semPlot\")", cex = 1.1)
    return(invisible(NULL))
  }
  tryCatch({
    semPlot::semPaths(fit, what = "std", whatLabels = "std", layout = "tree2", rotation = 2,
                      style = "lisrel", residuals = FALSE, intercepts = FALSE, thresholds = FALSE,
                      edge.label.cex = 0.9, sizeMan = 8, sizeLat = 10, nCharNodes = 0,
                      edge.color = "#2c3e50", fade = FALSE, mar = c(3, 3, 4, 3))
    graphics::title(main = title, line = 1)
  }, error = function(e) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, paste("Path diagram failed:", conditionMessage(e)), cex = 1)
  })
  invisible(NULL)
}
