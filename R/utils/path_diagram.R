# Path Diagram Utilities
# Custom path diagram drawing functions inspired by Kenny's APIMeM

#' Draw APIM path diagram
#' @param fit lavaan fit object
#' @param var_names List with X, M, Y variable names
#' @param partner_labels Labels for partners (e.g., c("Person 1", "Person 2"))
#' @return Plot
draw_apim_diagram <- function(fit, var_names = NULL, partner_labels = c("Person 1", "Person 2")) {
  
  # Extract parameter estimates
  params <- lavaan::parameterEstimates(fit, standardized = TRUE)
  
  # Helper function to get estimate
  get_est <- function(lhs, op, rhs) {
    row <- params[params$lhs == lhs & params$op == op & params$rhs == rhs, ]
    if (nrow(row) > 0) {
      list(
        est = row$std.all[1],
        pval = row$pvalue[1],
        sig = row$pvalue[1] < 0.05
      )
    } else {
      list(est = NA, pval = NA, sig = FALSE)
    }
  }
  
  # Set up plot
  par(mar = c(1, 1, 3, 1))
  plot(0, 0, type = "n", xlim = c(0, 100), ylim = c(0, 100), 
       xlab = "", ylab = "", axes = FALSE, asp = 1)
  
  # Define positions
  x1_pos <- c(15, 75)
  x2_pos <- c(15, 25)
  y1_pos <- c(85, 75)
  y2_pos <- c(85, 25)
  
  # Draw boxes
  rect_width <- 12
  rect_height <- 8
  
  # X variables
  rect(x1_pos[1] - rect_width/2, x1_pos[2] - rect_height/2,
       x1_pos[1] + rect_width/2, x1_pos[2] + rect_height/2,
       col = "lightblue", border = "black", lwd = 2)
  text(x1_pos[1], x1_pos[2], "X", cex = 1.5, font = 2)
  text(x1_pos[1], x1_pos[2] - 12, partner_labels[1], cex = 0.8)
  
  rect(x2_pos[1] - rect_width/2, x2_pos[2] - rect_height/2,
       x2_pos[1] + rect_width/2, x2_pos[2] + rect_height/2,
       col = "lightblue", border = "black", lwd = 2)
  text(x2_pos[1], x2_pos[2], "X", cex = 1.5, font = 2)
  text(x2_pos[1], x2_pos[2] + 12, partner_labels[2], cex = 0.8)
  
  # Y variables
  rect(y1_pos[1] - rect_width/2, y1_pos[2] - rect_height/2,
       y1_pos[1] + rect_width/2, y1_pos[2] + rect_height/2,
       col = "lightgreen", border = "black", lwd = 2)
  text(y1_pos[1], y1_pos[2], "Y", cex = 1.5, font = 2)
  text(y1_pos[1], y1_pos[2] - 12, partner_labels[1], cex = 0.8)
  
  rect(y2_pos[1] - rect_width/2, y2_pos[2] - rect_height/2,
       y2_pos[1] + rect_width/2, y2_pos[2] + rect_height/2,
       col = "lightgreen", border = "black", lwd = 2)
  text(y2_pos[1], y2_pos[2], "Y", cex = 1.5, font = 2)
  text(y2_pos[1], y2_pos[2] + 12, partner_labels[2], cex = 0.8)
  
  # Draw arrows with estimates
  draw_arrow_with_label <- function(x0, y0, x1, y1, label, sig = FALSE) {
    arrows(x0, y0, x1, y1, length = 0.15, lwd = if(sig) 3 else 2,
           col = if(sig) "red" else "black")
    
    # Calculate label position (midpoint)
    mid_x <- (x0 + x1) / 2
    mid_y <- (y0 + y1) / 2
    
    # Adjust position based on arrow direction
    offset <- 3
    if (abs(y1 - y0) > abs(x1 - x0)) {
      # Vertical arrow
      text(mid_x + offset, mid_y, label, cex = 0.9, font = if(sig) 2 else 1)
    } else {
      # Horizontal arrow
      text(mid_x, mid_y + offset, label, cex = 0.9, font = if(sig) 2 else 1)
    }
  }
  
  # Try to detect variable names from model
  var_list <- lavaan::lavNames(fit)
  
  # Actor effects (X1 -> Y1, X2 -> Y2)
  if (length(var_list) >= 4) {
    # Simplified: assume first 2 are X, last 2 are Y
    # This is a basic implementation
    draw_arrow_with_label(
      x1_pos[1] + rect_width/2, x1_pos[2],
      y1_pos[1] - rect_width/2, y1_pos[2],
      "a", sig = TRUE
    )
    
    draw_arrow_with_label(
      x2_pos[1] + rect_width/2, x2_pos[2],
      y2_pos[1] - rect_width/2, y2_pos[2],
      "a", sig = TRUE
    )
    
    # Partner effects (X1 -> Y2, X2 -> Y1)
    draw_arrow_with_label(
      x1_pos[1] + rect_width/2, x1_pos[2] - 5,
      y2_pos[1] - rect_width/2, y2_pos[2] + 5,
      "p", sig = FALSE
    )
    
    draw_arrow_with_label(
      x2_pos[1] + rect_width/2, x2_pos[2] + 5,
      y1_pos[1] - rect_width/2, y1_pos[2] - 5,
      "p", sig = FALSE
    )
  }
  
  # Draw correlations
  # X1 <-> X2
  lines(c(x1_pos[1], x2_pos[1]), c(x1_pos[2] - rect_height/2 - 2, x2_pos[2] + rect_height/2 + 2),
        lty = 2, lwd = 2, col = "blue")
  
  # Y1 <-> Y2
  lines(c(y1_pos[1], y2_pos[1]), c(y1_pos[2] - rect_height/2 - 2, y2_pos[2] + rect_height/2 + 2),
        lty = 2, lwd = 2, col = "blue")
  
  # Title
  title(main = "APIM Path Diagram (Standardized Estimates)", 
        cex.main = 1.5, font.main = 2)
  
  # Legend
  legend("topright", 
         legend = c("Actor effect", "Partner effect", "Correlation", "Significant (p<.05)"),
         lty = c(1, 1, 2, 1),
         lwd = c(2, 2, 2, 3),
         col = c("black", "black", "blue", "red"),
         cex = 0.8,
         bg = "white")
}

#' Draw generic SEM path diagram using semPlot
#' @param fit lavaan fit object
#' @param title Plot title
#' @return Plot
draw_sem_diagram <- function(fit, title = "SEM Path Diagram") {
  
  # Check if semPlot is available
  if (!requireNamespace("semPlot", quietly = TRUE)) {
    plot(1, type = "n", xlab = "", ylab = "", main = "Need to install semPlot package", axes = FALSE)
    text(1, 1, "Please run: install.packages('semPlot')", cex = 1.2)
    return(invisible(NULL))
  }
  
  tryCatch({
    semPlot::semPaths(
      fit,
      what = "std",
      layout = "tree2",
      edge.label.cex = 1.0,
      sizeMan = 10,
      sizeLat = 12,
      style = "lisrel",
      curve = 1.5,
      nCharNodes = 0,
      nCharEdges = 0,
      title = TRUE,
      ThreshAtSide = FALSE,
      rotation = 2,
      intercepts = FALSE,
      residuals = TRUE,
      exoCov = TRUE,
      mar = c(3, 3, 3, 3),
      border.width = 1.5,
      edge.width = 1.5,
      label.cex = 1.2,
      edge.color = "black"
    )
    
    title(main = title, line = 1, cex.main = 1.5)
    
  }, error = function(e) {
    plot(1, type = "n", xlab = "", ylab = "", main = "Path DiagramGeneration failed", axes = FALSE)
    text(1, 1, sprintf("Error: %s", e$message), cex = 1)
  })
}

#' Detect model type and draw appropriate diagram
#' @param fit lavaan fit object
#' @param model_name Model name
#' @return Plot
draw_auto_diagram <- function(fit, model_name = "Model") {
  
  # Get variable names
  var_names <- lavaan::lavNames(fit)
  
  # Try to detect APIM pattern
  is_apim <- length(var_names) >= 4 && 
             any(grepl("1$|2$|_1|_2", var_names))
  
  if (is_apim) {
    draw_apim_diagram(fit)
  } else {
    draw_sem_diagram(fit, title = model_name)
  }
}
