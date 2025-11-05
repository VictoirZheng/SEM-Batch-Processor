# Variable Mapping Utilities
# Provides intelligent variable mapping for SEM model templates

#' Extract variable placeholders from model template using {{}} syntax
#' @param model_text Character string containing the model syntax
#' @return List with placeholder information
extract_placeholders <- function(model_text) {
  if (is.null(model_text) || nchar(trimws(model_text)) == 0) {
    return(list(
      placeholders = character(0),
      placeholder_info = list(),
      count = 0,
      description = "No model template loaded"
    ))
  }
  
  # Extract placeholders using {{variable}} pattern
  placeholder_pattern <- "\\{\\{([^}]+)\\}\\}"
  matches <- gregexpr(placeholder_pattern, model_text, perl = TRUE)
  
  if (matches[[1]][1] == -1) {
    return(list(
      placeholders = character(0),
      placeholder_info = list(),
      count = 0,
      description = "No {{placeholders}} found in template"
    ))
  }
  
  # Extract placeholder names
  placeholder_texts <- regmatches(model_text, matches)[[1]]
  placeholder_names <- gsub("\\{\\{|\\}\\}", "", placeholder_texts)
  
  # Remove duplicates while preserving order
  unique_placeholders <- unique(placeholder_names)
  
  # Parse placeholder info (name:type format)
  placeholder_info <- lapply(unique_placeholders, function(ph) {
    # Check if placeholder has type annotation (e.g., {{outcome:Y}})
    if (grepl(":", ph)) {
      parts <- strsplit(ph, ":")[[1]]
      list(
        name = trimws(parts[1]),
        type = trimws(parts[2]),
        display_name = trimws(parts[1])
      )
    } else {
      # Infer type from name
      type <- infer_variable_type(ph)
      list(
        name = trimws(ph),
        type = type,
        display_name = trimws(ph)
      )
    }
  })
  
  names(placeholder_info) <- sapply(placeholder_info, function(x) x$name)
  
  # Group by type
  types <- sapply(placeholder_info, function(x) x$type)
  type_groups <- split(names(placeholder_info), types)
  
  list(
    placeholders = names(placeholder_info),
    placeholder_info = placeholder_info,
    type_groups = type_groups,
    count = length(placeholder_info),
    description = sprintf("Found %d {{placeholders}}", length(placeholder_info))
  )
}

#' Infer variable type from name
#' @param name Variable name
#' @return Type string
infer_variable_type <- function(name) {
  name_lower <- tolower(name)
  
  if (grepl("^[Yy]|outcome|dependent|dv", name_lower)) {
    return("outcome")
  } else if (grepl("^[Xx]|predictor|independent|iv", name_lower)) {
    return("predictor")
  } else if (grepl("^[Mm]|mediator|med", name_lower)) {
    return("mediator")
  } else if (grepl("^[Zz]|moderator|mod", name_lower)) {
    return("moderator")
  } else if (grepl("^[Ff]|factor|latent", name_lower)) {
    return("latent")
  } else if (grepl("item|indicator|observed", name_lower)) {
    return("observed")
  } else {
    return("other")
  }
}

#' Apply variable mapping to model template
#' @param model_text Original model template with {{}} placeholders
#' @param mapping Named vector of mappings (placeholder = actual_variable)
#' @return Modified model text with placeholders replaced
apply_mapping <- function(model_text, mapping) {
  if (is.null(model_text) || length(mapping) == 0) {
    return(model_text)
  }
  
  result <- model_text
  
  # Replace each {{placeholder}} with actual variable
  for (placeholder in names(mapping)) {
    actual_var <- mapping[[placeholder]]
    if (!is.null(actual_var) && nchar(actual_var) > 0) {
      # Replace {{placeholder}} with actual variable name
      pattern <- paste0("\\{\\{", placeholder, "\\}\\}")
      result <- gsub(pattern, actual_var, result, perl = TRUE)
    }
  }
  
  result
}

#' Validate variable mapping completeness
#' @param placeholders Character vector of placeholders
#' @param mapping Named vector of mappings
#' @param available_vars Character vector of available variables in data
#' @return List with validation results
validate_mapping <- function(placeholders, mapping, available_vars = NULL) {
  unmapped <- setdiff(placeholders, names(mapping))
  mapped <- intersect(placeholders, names(mapping))
  
  # Check if mapped variables exist in data
  invalid <- character(0)
  if (!is.null(available_vars)) {
    mapped_values <- mapping[mapped]
    invalid <- setdiff(mapped_values, available_vars)
  }
  
  is_complete <- length(unmapped) == 0
  is_valid <- length(invalid) == 0
  
  list(
    is_complete = is_complete,
    is_valid = is_valid,
    unmapped = unmapped,
    mapped = mapped,
    invalid = invalid,
    completion_rate = length(mapped) / length(placeholders) * 100,
    message = if (is_complete && is_valid) {
      "✓ All variables mapped correctly"
    } else if (!is_complete) {
      sprintf("⚠ %d variables need mapping", length(unmapped))
    } else {
      sprintf("✗ %d mapped variables not found in data", length(invalid))
    }
  )
}

#' Suggest automatic mappings based on variable names and types
#' @param placeholder_info List of placeholder information
#' @param available_vars Character vector of available variables
#' @return Named vector of suggested mappings
suggest_mappings <- function(placeholder_info, available_vars) {
  if (length(placeholder_info) == 0 || length(available_vars) == 0) {
    return(character(0))
  }
  
  suggestions <- character(0)
  
  for (ph_name in names(placeholder_info)) {
    ph <- placeholder_info[[ph_name]]
    
    # Try exact match (case-insensitive)
    exact_match <- available_vars[tolower(available_vars) == tolower(ph$name)]
    if (length(exact_match) > 0) {
      suggestions[ph$name] <- exact_match[1]
      next
    }
    
    # Try type-based matching
    type_matches <- character(0)
    if (ph$type == "outcome") {
      type_matches <- grep("^[Yy]|outcome|dependent|dv", available_vars, value = TRUE, ignore.case = TRUE)
    } else if (ph$type == "predictor") {
      type_matches <- grep("^[Xx]|predictor|independent|iv", available_vars, value = TRUE, ignore.case = TRUE)
    } else if (ph$type == "mediator") {
      type_matches <- grep("^[Mm]|mediator|med", available_vars, value = TRUE, ignore.case = TRUE)
    } else if (ph$type == "moderator") {
      type_matches <- grep("^[Zz]|moderator|mod", available_vars, value = TRUE, ignore.case = TRUE)
    }
    
    if (length(type_matches) > 0) {
      suggestions[ph$name] <- type_matches[1]
      next
    }
    
    # Try partial match
    partial_matches <- grep(ph$name, available_vars, ignore.case = TRUE, value = TRUE)
    if (length(partial_matches) > 0) {
      suggestions[ph$name] <- partial_matches[1]
      next
    }
    
    # Try reverse partial match
    reverse_matches <- available_vars[grepl(tolower(ph$name), tolower(available_vars))]
    if (length(reverse_matches) > 0) {
      suggestions[ph$name] <- reverse_matches[1]
    }
  }
  
  suggestions
}

#' Generate mapping UI elements
#' @param placeholder_info List of placeholder information
#' @param type_groups List of placeholders grouped by type
#' @param available_vars Character vector of available variables
#' @param current_mapping Named vector of current mappings
#' @return Shiny UI elements
generate_mapping_ui <- function(placeholder_info, type_groups, available_vars, current_mapping = NULL) {
  if (length(placeholder_info) == 0) {
    return(div(
      class = "alert alert-info",
      icon("info-circle"),
      " No detection in template {{Placeholder}}。",
      br(), br(),
      "Tip：Use {{variable}} format definitionPlaceholder，For example：",
      br(),
      tags$code("{{outcome}} ~ {{predictor}}")
    ))
  }
  
  if (is.null(available_vars) || length(available_vars) == 0) {
    return(div(
      class = "alert alert-warning",
      icon("exclamation-triangle"),
      " Please firstUpload Datato performVariablemapping。"
    ))
  }
  
  # Create mapping inputs grouped by type
  ui_elements <- list()
  
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
  
  # Group placeholders by type
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
    
    # Create select inputs for each placeholder
    for (var_name in type_vars) {
      ph <- placeholder_info[[var_name]]
      current_value <- if (!is.null(current_mapping)) current_mapping[[var_name]] else NULL
      
      ui_elements[[length(ui_elements) + 1]] <- div(
        style = "margin-bottom: 10px;",
        fluidRow(
          column(
            width = 4,
            tags$label(
              sprintf("{{%s}}", ph$display_name),
              style = "font-family: monospace; font-weight: bold; color: #d73925;"
            )
          ),
          column(
            width = 1,
            icon("arrow-right", style = "margin-top: 8px; color: #999;")
          ),
          column(
            width = 7,
            selectInput(
              paste0("map_", var_name),
              label = NULL,
              choices = c("Notmapping" = "", available_vars),
              selected = current_value,
              width = "100%"
            )
          )
        )
      )
    }
  }
  
  tagList(ui_elements)
}
