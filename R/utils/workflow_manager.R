# Workflow Manager
# Manages the step-by-step workflow for SEM batch processing

#' Create workflow state tracker
#' @return List with workflow state management functions
create_workflow_tracker <- function() {
  state <- list(
    current_step = 1,
    steps = list(
      list(id = 1, name = "Upload Data", status = "pending", icon = "database"),
      list(id = 2, name = "Select Template", status = "pending", icon = "file-code"),
      list(id = 3, name = "mappingVariable", status = "pending", icon = "exchange-alt"),
      list(id = 4, name = "batch analysis", status = "pending", icon = "tasks"),
      list(id = 5, name = "View results", status = "pending", icon = "chart-bar")
    ),
    data_loaded = FALSE,
    template_loaded = FALSE,
    variables_mapped = FALSE,
    analysis_complete = FALSE
  )
  
  list(
    get_state = function() state,
    
    update_step = function(step_id, status) {
      if (step_id >= 1 && step_id <= length(state$steps)) {
        state$steps[[step_id]]$status <<- status
        if (status == "complete") {
          state$current_step <<- min(step_id + 1, length(state$steps))
        }
      }
    },
    
    set_data_loaded = function(loaded) {
      state$data_loaded <<- loaded
      if (loaded) {
        state$steps[[1]]$status <<- "complete"
        state$current_step <<- 2
      }
    },
    
    set_template_loaded = function(loaded) {
      state$template_loaded <<- loaded
      if (loaded) {
        state$steps[[2]]$status <<- "complete"
        state$current_step <<- 3
      }
    },
    
    set_variables_mapped = function(mapped) {
      state$variables_mapped <<- mapped
      if (mapped) {
        state$steps[[3]]$status <<- "complete"
        state$current_step <<- 4
      }
    },
    
    set_analysis_complete = function(complete) {
      state$analysis_complete <<- complete
      if (complete) {
        state$steps[[4]]$status <<- "complete"
        state$current_step <<- 5
      }
    },
    
    can_proceed_to = function(step_id) {
      if (step_id == 1) return(TRUE)
      if (step_id == 2) return(state$data_loaded)
      if (step_id == 3) return(state$data_loaded && state$template_loaded)
      if (step_id == 4) return(state$data_loaded && state$template_loaded && state$variables_mapped)
      if (step_id == 5) return(state$analysis_complete)
      return(FALSE)
    },
    
    get_next_step_message = function() {
      if (!state$data_loaded) {
        return("Please firstUpload Data")
      } else if (!state$template_loaded) {
        return("Please select or edit template")
      } else if (!state$variables_mapped) {
        return("Please mapVariable")
      } else if (!state$analysis_complete) {
        return("PreparingHomebatch analysis")
      } else {
        return("Analysis completed，View results")
      }
    }
  )
}

#' Generate workflow progress UI
#' @param workflow_state Workflow state object
#' @return Shiny UI elements
generate_workflow_ui <- function(workflow_state) {
  state <- workflow_state$get_state()
  
  steps_ui <- lapply(seq_along(state$steps), function(i) {
    step <- state$steps[[i]]
    
    # Determine step class and icon
    step_class <- switch(step$status,
      complete = "workflow-step-complete",
      active = "workflow-step-active",
      pending = "workflow-step-pending",
      "workflow-step-pending"
    )
    
    icon_class <- switch(step$status,
      complete = "check-circle",
      active = "circle",
      pending = "circle",
      "circle"
    )
    
    color <- switch(step$status,
      complete = "green",
      active = "blue",
      pending = "yellow",
      "yellow"
    )
    
    is_current <- (i == state$current_step)
    
    div(
      class = paste("workflow-step", step_class, if(is_current) "workflow-step-current"),
      style = sprintf("border-left: 4px solid %s; padding: 10px; margin-bottom: 10px;", color),
      fluidRow(
        column(
          width = 2,
          icon(icon_class, style = sprintf("font-size: 24px; color: %s;", color))
        ),
        column(
          width = 8,
          h5(sprintf("%d. %s", i, step$name), style = "margin: 0;"),
          if (is_current) {
            p(workflow_state$get_next_step_message(), 
              style = "margin: 5px 0 0 0; color: #666; font-size: 12px;")
          }
        ),
        column(
          width = 2,
          if (step$status == "complete") {
            icon("check", style = "color: green; font-size: 20px;")
          }
        )
      )
    )
  })
  
  tagList(steps_ui)
}
