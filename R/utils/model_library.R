# Model Library Manager
# Manages multiple models for batch analysis

#' Create model library manager
#' @return List with model library management functions
create_model_library <- function() {
  models <- list()
  
  list(
    # Add a model to the library
    add_model = function(name, template, mapping, final_syntax) {
      model_id <- paste0("model_", length(models) + 1)
      models[[model_id]] <<- list(
        id = model_id,
        name = name,
        template = template,
        mapping = mapping,
        final_syntax = final_syntax,
        created_at = Sys.time()
      )
      return(model_id)
    },
    
    # Get all models
    get_all = function() {
      models
    },
    
    # Get a specific model
    get_model = function(model_id) {
      models[[model_id]]
    },
    
    # Remove a model
    remove_model = function(model_id) {
      models[[model_id]] <<- NULL
    },
    
    # Clear all models
    clear_all = function() {
      models <<- list()
    },
    
    # Get model count
    count = function() {
      length(models)
    },
    
    # Generate models from combinations
    generate_combinations = function(template, placeholder_info, multi_mapping) {
      # multi_mapping: list(placeholder_name = c(var1, var2, var3))
      
      # Get all placeholder names
      ph_names <- names(multi_mapping)
      
      # Create all combinations
      combinations <- expand.grid(multi_mapping, stringsAsFactors = FALSE)
      
      # Generate models
      generated_models <- list()
      for (i in 1:nrow(combinations)) {
        mapping <- as.list(combinations[i, ])
        names(mapping) <- ph_names
        
        # Apply mapping
        final_syntax <- template
        for (ph in ph_names) {
          pattern <- paste0("\\{\\{", ph, "\\}\\}")
          replacement <- mapping[[ph]]
          # Guard against NULL / length-0 replacements which can cause
          # "argument length is zero" errors in downstream checks
          if (is.null(replacement) || length(replacement) == 0 || (length(replacement) == 1 && is.na(replacement))) {
            replacement <- ""
          }
          final_syntax <- gsub(pattern, replacement, final_syntax, perl = TRUE)
        }

        # Remove any lines that still contain unresolved placeholders
        # and trim excessive blank lines
        lines <- unlist(strsplit(final_syntax, "\n", fixed = TRUE))
        if (length(lines) == 0) {
          next
        }
        lines <- lines[!grepl("\\{\\{[^}]+\\}\\}", lines)]
        # If everything was removed or only whitespace remains, skip this combo
        if (length(lines) == 0) {
          next
        }
        # Collapse back to single string and normalize multiple blank lines
        final_syntax <- paste(lines, collapse = "\n")
        final_syntax <- gsub("\n{3,}", "\n\n", final_syntax)
        if (is.null(final_syntax) || length(final_syntax) == 0 || nchar(trimws(final_syntax)) == 0) {
          next
        }
        
        # Create model name
        mapping_str <- paste(sapply(ph_names, function(p) {
          sprintf("%s=%s", p, mapping[[p]])
        }), collapse = ", ")
        model_name <- sprintf("combinations%d: %s", i, mapping_str)
        
        # Add to library
        model_id <- paste0("combo_", i)
        generated_models[[model_id]] <- list(
          id = model_id,
          name = model_name,
          template = template,
          mapping = mapping,
          final_syntax = final_syntax,
          created_at = Sys.time(),
          is_combination = TRUE
        )
      }
      
      # Add all generated models to library
      models <<- c(models, generated_models)
      
      return(length(generated_models))
    }
  )
}

#' Generate model library UI
#' @param model_library Model library object
#' @return Shiny UI elements
generate_model_library_ui <- function(model_library) {
  models <- model_library$get_all()
  
  if (length(models) == 0) {
    return(div(
      class = "alert alert-info",
      icon("info-circle"),
      " Model Libraryis empty。Please add models or generate combinations。"
    ))
  }
  
  # Create model cards
  model_cards <- lapply(names(models), function(model_id) {
    model <- models[[model_id]]
    
    div(
      class = "model-card",
      style = "border: 1px solid #ddd; border-radius: 4px; padding: 10px; margin-bottom: 10px; background: #f9f9f9;",
      fluidRow(
        column(
          width = 8,
          h5(
            icon("cube"),
            model$name,
            style = "margin: 0; color: #3c8dbc;"
          ),
          p(
            style = "margin: 5px 0; font-size: 11px; color: #666;",
            sprintf("Created Time: %s", format(model$created_at, "%Y-%m-%d %H:%M:%S"))
          ),
          tags$details(
            tags$summary("View model code", style = "cursor: pointer; color: #3c8dbc;"),
            tags$pre(
              style = "background: #fff; padding: 10px; margin-top: 5px; font-size: 11px; max-height: 150px; overflow-y: auto;",
              model$final_syntax
            )
          )
        ),
        column(
          width = 4,
          style = "text-align: right;",
          actionButton(
            paste0("edit_model_", model_id),
            "",
            icon = icon("edit"),
            class = "btn-info btn-xs",
            style = "margin-top: 5px; margin-right: 3px;",
            title = "Edit"
          ),
          actionButton(
            paste0("rename_model_", model_id),
            "",
            icon = icon("i-cursor"),
            class = "btn-warning btn-xs",
            style = "margin-top: 5px; margin-right: 3px;",
            title = "Rename"
          ),
          actionButton(
            paste0("remove_model_", model_id),
            "",
            icon = icon("trash"),
            class = "btn-danger btn-xs",
            style = "margin-top: 5px;",
            title = "Delete"
          )
        )
      )
    )
  })
  
  tagList(
    div(
      style = "margin-bottom: 15px; padding: 10px; background: #d9edf7; border-radius: 4px;",
      h5(
        icon("list"),
        sprintf("Model Library (%d Models)", length(models)),
        style = "margin: 0; color: #31708f;"
      )
    ),
    model_cards,
    hr(),
    actionButton(
      "clear_all_models",
      "ClearModel Library",
      icon = icon("trash-alt"),
      class = "btn-warning btn-sm"
    )
  )
}

#' Generate multi-select mapping UI for combinations
#' @param placeholder_info Placeholder information
#' @param available_vars Available variables
#' @return Shiny UI elements
generate_multi_mapping_ui <- function(placeholder_info, available_vars) {
  if (length(placeholder_info) == 0) {
    return(div(
      class = "alert alert-info",
      "Please first select containing {{Placeholder}} template"
    ))
  }
  
  if (is.null(available_vars) || length(available_vars) == 0) {
    return(div(
      class = "alert alert-warning",
      "Please firstUpload Data"
    ))
  }
  
  # Type labels
  type_labels <- list(
    outcome = list(label = "resultsVariable", icon = "bullseye", color = "#dd4b39"),
    predictor = list(label = "predictionVariable", icon = "arrow-right", color = "#00a65a"),
    mediator = list(label = "InmediationVariable", icon = "exchange-alt", color = "#f39c12"),
    moderator = list(label = "moderationVariable", icon = "sliders-h", color = "#605ca8"),
    latent = list(label = "LatentVariable", icon = "circle", color = "#3c8dbc"),
    observed = list(label = "ObservedVariable", icon = "square", color = "#00c0ef"),
    other = list(label = "OtherVariable", icon = "question-circle", color = "#999")
  )
  
  # Group by type
  types <- sapply(placeholder_info, function(x) x$type)
  type_groups <- split(names(placeholder_info), types)
  
  ui_elements <- list()
  
  # Add instruction
  ui_elements[[length(ui_elements) + 1]] <- div(
    class = "alert alert-info",
    style = "margin-bottom: 15px;",
    icon("info-circle"),
    " For eachPlaceholderSelect one or moreVariable。System will automaticallyGenerateAll possiblecombinations。"
  )
  
  # Create multi-select inputs for each placeholder
  for (type_name in names(type_groups)) {
    type_vars <- type_groups[[type_name]]
    if (length(type_vars) == 0) next
    
    type_info <- type_labels[[type_name]]
    if (is.null(type_info)) {
      type_info <- list(label = type_name, icon = "tag", color = "#999")
    }
    
    # Type header
    ui_elements[[length(ui_elements) + 1]] <- h5(
      icon(type_info$icon),
      type_info$label,
      style = sprintf("margin-top: 15px; color: %s;", type_info$color)
    )
    
    # Create multi-select for each placeholder
    for (var_name in type_vars) {
      ph <- placeholder_info[[var_name]]
      
      ui_elements[[length(ui_elements) + 1]] <- div(
        style = "margin-bottom: 15px;",
        tags$label(
          sprintf("{{%s}}", ph$display_name),
          style = "font-family: monospace; font-weight: bold; color: #d73925;"
        ),
        selectInput(
          paste0("multi_map_", var_name),
          label = NULL,
          choices = available_vars,
          selected = NULL,
          multiple = TRUE,
          width = "100%"
        ),
        tags$small(
          class = "text-muted",
          "Can select multipleVariable"
        )
      )
    }
  }
  
  tagList(ui_elements)
}
