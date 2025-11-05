# Main Server Logic
# Handles all server-side interactions for the SEM Batch Processor

#' Create main server function
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
create_main_server <- function(input, output, session) {
  
  # Load utilities
  if (file.exists("R/utils/workflow_manager.R")) {
    source("R/utils/workflow_manager.R")
  }
  if (file.exists("R/utils/batch_processor.R")) {
    source("R/utils/batch_processor.R")
  }
  if (file.exists("R/utils/model_library.R")) {
    source("R/utils/model_library.R")
  }
  
  # Reactive values to store application state
  values <- reactiveValues(
    data = NULL,
    data_info = NULL,
    model_template = NULL,
    model_final = NULL,  # Final model after mapping
    variable_mapping = NULL,
    batch_results = NULL,
    current_engine = NULL,
    analysis_progress = 0,
    analysis_message = "Preparing...",
    analysis_running = FALSE,
    analysis_start_time = NULL,
    analysis_elapsed_time = 0,
    model_library_trigger = 0  # Force model library UI update
  )
  
  # Initialize workflow tracker
  workflow <- create_workflow_tracker()
  progress_tracker <- create_progress_tracker()
  
  # Initialize model library
  model_lib <- create_model_library()
  
  # Initialize engine manager
  engine_mgr <- tryCatch({
    if (exists("get_engine_manager", mode = "function")) {
      get_engine_manager()
    } else {
      if (file.exists("R/engines/engine_manager.R")) {
        source("R/engines/engine_manager.R")
        get_engine_manager()
      } else {
        NULL
      }
    }
  }, error = function(e) {
    log_warning(sprintf("Failed to initialize engine manager: %s", e$message))
    NULL
  })
  
  # Update current engine
  if (!is.null(engine_mgr)) {
    values$current_engine <- engine_mgr$current_engine$name
  }
  
  # ============================================================================
  # Home Page - System Status
  # ============================================================================
  
  output$engine_status <- renderValueBox({
    # Get current engine name from header selector or engine manager
    current_engine_name <- if (!is.null(input$header_engine_select)) {
      if (input$header_engine_select == "mplus") "Mplus" else "R/lavaan"
    } else if (!is.null(engine_mgr) && !is.null(engine_mgr$current_engine)) {
      engine_mgr$current_engine$name
    } else {
      "R/lavaan"
    }
    
    valueBox(
      value = current_engine_name,
      subtitle = "Current Engine",
      icon = icon("cogs"),
      color = "blue"
    )
  })
  
  output$data_status <- renderValueBox({
    if (!is.null(values$data)) {
      valueBox(
        value = sprintf("%d × %d", nrow(values$data), ncol(values$data)),
        subtitle = "Data Status",
        icon = icon("database"),
        color = "green"
      )
    } else {
      valueBox(
        value = "Not Loaded",
        subtitle = "Data Status",
        icon = icon("database"), 
        color = "yellow"
      )
    }
  })
  
  output$model_status <- renderValueBox({
    if (!is.null(values$model_template)) {
      valueBox(
        value = "Configured",
        subtitle = "Model Status",
        icon = icon("project-diagram"),
        color = "green"
      )
    } else {
      valueBox(
        value = "Not Configured", 
        subtitle = "Model Status",
        icon = icon("project-diagram"),
        color = "red"
      )
    }
  })
  
  output$recent_activity <- renderUI({
    if (!is.null(engine_mgr)) {
      tags$ul(
        tags$li("Application initialized"),
        tags$li(sprintf("Detected %d available engines", length(engine_mgr$available_engines))),
        tags$li(sprintf("Current Engine: %s", engine_mgr$current_engine$name)),
        if (!is.null(values$data)) {
          tags$li(sprintf("LoadedData: %d rows %d columns", nrow(values$data), ncol(values$data)))
        },
        if (!is.null(values$model_template)) {
          tags$li("Model templates configured")
        }
      )
    } else {
      tags$ul(
        tags$li("Application initialized"),
        tags$li("Engine ManagementProcessorNot Loaded"),
        tags$li("Please refresh the page")
      )
    }
  })
  
  # Start workflow button
  observeEvent(input$start_workflow, {
    updateTabItems(session, "sidebar_menu", "data")
    showNotification("HomeData ManagementStep", type = "message")
  })
  
  # ============================================================================
  # Data Management Page
  # ============================================================================
  
  # File upload handling
  observeEvent(input$data_file, {
    req(input$data_file)
    tryCatch({
      file_path <- input$data_file$datapath
      file_ext <- tools::file_ext(input$data_file$name)
      
      log_info("Loading data file", 
               filename = input$data_file$name, 
               size = input$data_file$size)
      
      # Read data based on file extension
      data <- switch(tolower(file_ext),
        csv = read.csv(file_path, stringsAsFactors = FALSE),
        sav = {
          if (requireNamespace("foreign", quietly = TRUE)) {
            foreign::read.spss(file_path, to.data.frame = TRUE)
          } else {
            stop("foreign package required for SPSS files")
          }
        },
        xlsx = {
          if (requireNamespace("readxl", quietly = TRUE)) {
            readxl::read_excel(file_path)
          } else {
            stop("readxl package required for Excel files")
          }
        },
        dta = {
          if (requireNamespace("foreign", quietly = TRUE)) {
            foreign::read.dta(file_path)
          } else {
            stop("foreign package required for Stata files")
          }
        },
        stop("Unsupported file format")
      )
      
      # Store data
      values$data <- as.data.frame(data)
      values$data_info <- list(
        filename = input$data_file$name,
        nrow = nrow(data),
        ncol = ncol(data),
        variables = names(data),
        uploaded_at = Sys.time()
      )
      
      log_info("Data loaded successfully", 
               nrow = nrow(data), 
               ncol = ncol(data))
      
      showNotification(
        sprintf("Data LoadingSuccess：%d rows %d columns", nrow(data), ncol(data)),
        type = "message"
      )
    }, error = function(e) {
      log_error("Data loading failed", error = e$message)
      showNotification(
        paste("Data LoadingFailed:", e$message),
        type = "error"
      )
    })
  })
  
  # Load data button
  observeEvent(input$load_data, {
    if (is.null(input$data_file)) {
      showNotification("Please firstSelectdata file", type = "warning")
      return()
    }
    # Trigger file processing (already handled by observeEvent above)
    showNotification("Processingdata file...", type = "message")
  })
  
  # Clear data button
  observeEvent(input$clear_data, {
    values$data <- NULL
    values$data_info <- NULL
    showNotification("Data cleared", type = "message")
  })
  
  # Data preview table
  output$data_preview <- renderDT({
    if (!is.null(values$data)) {
      datatable(
        values$data,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          autoWidth = TRUE,
          columnDefs = list(
            list(className = 'dt-center dt-nowrap', targets = '_all')
          )
        ),
        caption = sprintf("Data Preview - %s (%d rows %d columns)", 
                         values$data_info$filename,
                         values$data_info$nrow,
                         values$data_info$ncol),
        class = 'cell-border stripe compact',
        style = 'bootstrap',
        rownames = FALSE
      ) %>%
        formatStyle(columns = colnames(values$data), lineHeight = '80%')
    } else {
      datatable(
        data.frame(Message = "PleaseUpload DataFile"),
        options = list(dom = 't'),
        caption = "No data available"
      )
    }
  })
  
  # Descriptive statistics
  output$descriptive_stats <- renderDT({
    if (!is.null(values$data)) {
      tryCatch({
        numeric_vars <- sapply(values$data, is.numeric)
        if (any(numeric_vars)) {
          numeric_data <- values$data[, numeric_vars, drop = FALSE]
          stats <- data.frame(
            Variable = names(numeric_data),
            N = sapply(numeric_data, function(x) sum(!is.na(x))),
            Missing = sapply(numeric_data, function(x) sum(is.na(x))),
            Mean = sapply(numeric_data, function(x) round(mean(x, na.rm = TRUE), 3)),
            SD = sapply(numeric_data, function(x) round(sd(x, na.rm = TRUE), 3)),
            Min = sapply(numeric_data, function(x) round(min(x, na.rm = TRUE), 3)),
            Max = sapply(numeric_data, function(x) round(max(x, na.rm = TRUE), 3)),
            stringsAsFactors = FALSE
          )
          datatable(
            stats,
            options = list(pageLength = 15, dom = 'Bfrtip'),
            caption = "Descriptive Statistics (Numeric onlyVariable)"
          )
        } else {
          datatable(
            data.frame(Message = "No numeric valuesVariable"),
            options = list(dom = 't')
          )
        }
      }, error = function(e) {
        datatable(
          data.frame(Error = paste("Statistical calculationError:", e$message)),
          options = list(dom = 't')
        )
      })
    } else {
      datatable(
        data.frame(Message = "Please upload data first"),
        options = list(dom = 't')
      )
    }
  })
  
  # Missing data analysis
  output$missing_data_plot <- renderPlot({
    if (!is.null(values$data)) {
      tryCatch({
        missing_pattern <- is.na(values$data)
        missing_counts <- colSums(missing_pattern)
        if (any(missing_counts > 0)) {
          missing_df <- data.frame(
            Variable = names(missing_counts),
            Missing_Count = missing_counts,
            Missing_Percent = round(missing_counts / nrow(values$data) * 100, 2)
          )
          missing_df <- missing_df[missing_df$Missing_Count > 0, ]
          if (nrow(missing_df) > 0) {
            ggplot(missing_df, aes(x = reorder(Variable, Missing_Percent), y = Missing_Percent)) +
              geom_bar(stat = "identity", fill = "orange", alpha = 0.7) +
              coord_flip() +
              labs(
                title = "Missing data analysis",
                x = "Variable",
                y = "Missing percentage (%)"
              ) +
              theme_minimal()
          } else {
            ggplot() + 
              annotate("text", x = 0.5, y = 0.5, label = "No missing data", size = 6) +
              theme_void()
          }
        } else {
          ggplot() + 
            annotate("text", x = 0.5, y = 0.5, label = "No missing data", size = 6) +
            theme_void()
        }
      }, error = function(e) {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = paste("PlotError:", e$message), size = 4) +
          theme_void()
      })
    } else {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = "Please upload data first", size = 6) +
        theme_void()
    }
  })
  
  output$missing_data_table <- renderDT({
    if (!is.null(values$data)) {
      missing_pattern <- is.na(values$data)
      missing_counts <- colSums(missing_pattern)
      missing_df <- data.frame(
        Variable = names(missing_counts),
        Missing_Count = missing_counts,
        Missing_Percent = round(missing_counts / nrow(values$data) * 100, 2),
        stringsAsFactors = FALSE
      )
      datatable(
        missing_df,
        options = list(pageLength = 10),
        caption = "Missing data statistics"
      )
    } else {
      datatable(
        data.frame(Message = "Please upload data first"),
        options = list(dom = 't')
      )
    }
  })
  
  # ============================================================================
  # Model Design Page
  # ============================================================================
  
  # Helper function to load template based on engine type
  load_template_by_engine <- function(template_name, engine_type) {
    # Determine template file path
    if (engine_type == "mplus") {
      template_file <- file.path("templates", "mplus", paste0(template_name, "_mplus.txt"))
    } else {
      # R/lavaan templates (hardcoded for now)
      return(switch(template_name,
        "basic_sem" = "# BasicSEMModel\n# Measurement model\n{{latent1}} =~ {{latent1_item1}} + {{latent1_item2}} + {{latent1_item3}}\n{{latent2}} =~ {{latent2_item1}} + {{latent2_item2}} + {{latent2_item3}}\n\n# Structural model\n{{latent2}} ~ {{latent1}}\n",
        "apim" = "# APIMModel (Actor-Partner Interdependence Model)\n# Actor effects\n{{outcome_actor}} ~ {{predictor_actor}}\n{{outcome_partner}} ~ {{predictor_partner}}\n\n# Partner effects\n{{outcome_actor}} ~ {{predictor_partner}}\n{{outcome_partner}} ~ {{predictor_actor}}\n\n# Covariances\n{{predictor_actor}} ~~ {{predictor_partner}}\n{{outcome_actor}} ~~ {{outcome_partner}}\n",
        "mediation" = "# Mediation model\n# Direct effects\n{{outcome}} ~ c*{{predictor}}\n{{mediator}} ~ a*{{predictor}}\n{{outcome}} ~ b*{{mediator}}\n\n# Indirect effect\nindirect := a*b\ntotal := c + (a*b)\n",
        "moderation" = "# Moderation model\n# Main effects\n{{outcome}} ~ {{predictor}} + {{moderator}}\n\n# Interaction\n{{outcome}} ~ {{predictor}}:{{moderator}}\n",
        ""
      ))
    }
    
    # Try to read Mplus template from file
    if (file.exists(template_file)) {
      return(paste(readLines(template_file), collapse = "\n"))
    } else {
      showNotification(
        sprintf("MplusTemplate file not found: %s", template_file),
        type = "warning"
      )
      return("")
    }
  }
  
  # Load template button (Main logic)
  observeEvent(input$load_template, {
    if (input$template_select == "custom") {
      showNotification("Please select a predefined template", type = "warning")
      return()
    }
    
    # Get current engine type
    engine_type <- input$header_engine_select
    if (is.null(engine_type)) engine_type <- "r"
    
    # Load template based on engine type
    template_content <- load_template_by_engine(input$template_select, engine_type)
    
    if (template_content != "") {
      updateTextAreaInput(session, "model_editor", value = template_content)
      values$model_template <- template_content
      values$variable_mapping <- NULL  # Clear previousmapping
      
      engine_label <- if (engine_type == "mplus") "Mplus" else "R/lavaan"
      showNotification(
        sprintf("Loaded %s Template (%sEngine)", input$template_select, engine_label),
        type = "message"
      )
    }
  })
  
  # Clear template button
  observeEvent(input$clear_template, {
    updateTextAreaInput(session, "model_editor", value = "")
    values$model_template <- NULL
    values$variable_mapping <- NULL
    showNotification("Template cleared", type = "message")
  })
  
  # Check if model has placeholders
  output$has_placeholders <- reactive({
    model_text <- input$model_editor
    if (is.null(model_text) || nchar(trimws(model_text)) == 0) {
      return(FALSE)
    }
    ph_info <- extract_placeholders(model_text)
    return(ph_info$count > 0)
  })
  outputOptions(output, "has_placeholders", suspendWhenHidden = FALSE)
  
  # Save template button
  observeEvent(input$save_template, {
    if (is.null(input$model_editor) || input$model_editor == "") {
      showNotification("Please firstEnter model code", type = "warning")
      return()
    }
    # For now, just save to reactive values
    values$model_template <- input$model_editor
    showNotification("Template saved to current session", type = "message")
  })
  
  # Model validation
  observeEvent(input$validate_model, {
    if (is.null(input$model_editor) || input$model_editor == "") {
      output$model_validation_output <- renderText("Please firstEnter model code")
      return()
    }
    tryCatch({
      # Try to parse with lavaan
      if (requireNamespace("lavaan", quietly = TRUE)) {
        # Clean model text
        model_text <- input$model_editor
        
        # Try to parse
        parsed <- lavaan::lavaanify(model_text, warn = FALSE)
        
        output$model_validation_output <- renderText(
          paste("✓ Model SyntaxValidationThrough\n",
                sprintf("Detected %d Parameter", nrow(parsed)),
                "\n\nTip：If modelpackageContains {{Placeholder}}，Please firstApplyVariablemapping")
        )
        showNotification("ModelValidationThrough", type = "message")
      } else {
        output$model_validation_output <- renderText("lavaan packageNot installed，Unable toValidationModel")
      }
    }, error = function(e) {
      # Provide more helpful error messages
      error_msg <- e$message
      
      # Enhanced error messages for common issues
      if (grepl("times", error_msg, ignore.case = TRUE)) {
        error_msg <- paste("Model SyntaxError：'times'Invalid value",
                          "\n\nThis usually indicates：",
                          "\n1. In modelpackageContains {{Placeholder}} But not yetmappingVariable",
                          "\n   → Solution：On the left'Variablemapping'AreaCompletedmapping，Then click'Applymapping'",
                          "\n\n2. Model SyntaxError（Check operators）",
                          "\n   → Correct format：",
                          "\n      Factor analysis：factor =~ item1 + item2",
                          "\n      Regression：outcome ~ predictor",
                          "\n      Covariance：var1 ~~ var2",
                          "\n\n3. VariableNamepackageContains special characters orEmptyFormat",
                          "\n   → VariableName should onlypackageContains letters、Numbers and underscores",
                          "\n\nOriginalErrorInformation：", error_msg)
      } else if (grepl("syntax", error_msg, ignore.case = TRUE)) {
        error_msg <- paste("Model SyntaxError",
                          "\n\nPlease check：",
                          "\n- Whether operators are correct（~, =~, ~~）",
                          "\n- Whether brackets match",
                          "\n- VariableWhether name is valid",
                          "\n\nOriginalError：", error_msg)
      }
      
      output$model_validation_output <- renderText(
        paste("✗ ModelValidationFailed\n\n", error_msg)
      )
      showNotification("ModelValidationFailed - Please see details", type = "error", duration = 5)
    })
  })
  
  # Model preview
  observeEvent(input$preview_model, {
    if (is.null(input$model_editor) || input$model_editor == "") {
      showNotification("Please firstEnter model code", type = "warning")
      return()
    }
    # For now, just show the model text
    showModal(modalDialog(
      title = "Model preview",
      tags$pre(input$model_editor),
      footer = modalButton("Close"),
      size = "l"
    ))
  })
  
  # Load variable mapper utilities
  if (file.exists("R/utils/variable_mapper.R")) {
    source("R/utils/variable_mapper.R")
  }
  
  # Extract placeholders when template changes
  placeholders_info <- reactive({
    model_text <- input$model_editor
    if (is.null(model_text) || nchar(trimws(model_text)) == 0) {
      return(list(placeholders = character(0), placeholder_info = list(), type_groups = list(), count = 0))
    }
    extract_placeholders(model_text)
  })
  
  # Variable mapping UI
  output$variable_mapping_ui <- renderUI({
    ph_info <- placeholders_info()
    available_vars <- if (!is.null(values$data)) names(values$data) else NULL
    
    if (ph_info$count == 0) {
      return(div(
        class = "alert alert-info",
        icon("info-circle"),
        " CurrentNo detection in template {{Placeholder}}。",
        br(), br(),
        "Tip：Use {{variable}} format definitionPlaceholder，For example：",
        br(),
        tags$code("{{outcome}} ~ {{predictor}}")
      ))
    }
    
    if (is.null(available_vars)) {
      return(div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        " Please upload data firstto performVariablemapping。"
      ))
    }
    
    generate_mapping_ui(
      ph_info$placeholder_info,
      ph_info$type_groups,
      available_vars,
      values$variable_mapping
    )
  })
  
  # Mapping validation message (real-time update)
  output$mapping_validation_message <- renderUI({
    ph_info <- placeholders_info()
    if (ph_info$count == 0 || is.null(values$data)) {
      return(NULL)
    }
    
    # Collect current mappings from inputs (real-time)
    current_mapping <- list()
    for (var in ph_info$placeholders) {
      input_id <- paste0("map_", var)
      if (!is.null(input[[input_id]]) && input[[input_id]] != "") {
        current_mapping[[var]] <- input[[input_id]]
      }
    }
    
    validation <- validate_mapping(
      ph_info$placeholders,
      current_mapping,
      names(values$data)
    )
    
    icon_class <- if (validation$is_complete && validation$is_valid) {
      "check-circle"
    } else if (!validation$is_complete) {
      "exclamation-circle"
    } else {
      "times-circle"
    }
    
    alert_class <- if (validation$is_complete && validation$is_valid) {
      "alert-success"
    } else if (!validation$is_complete) {
      "alert-warning"
    } else {
      "alert-danger"
    }
    
    div(
      class = paste("alert", alert_class),
      style = "margin: 0;",
      icon(icon_class),
      sprintf(" %s (%.0f%% Completed)", validation$message, validation$completion_rate)
    )
  })
  
  # Apply mapping and save button
  observeEvent(input$apply_and_save, {
    ph_info <- placeholders_info()
    if (ph_info$count == 0) {
      showNotification("No needmappingofVariable", type = "warning")
      return()
    }
    
    # Get model name
    model_name <- input$model_name_mapping
    if (is.null(model_name) || nchar(trimws(model_name)) == 0) {
      # Generate default name
      model_count <- length(model_lib$get_all())
      model_name <- sprintf("Model_%d", model_count + 1)
    }
    
    # Collect mappings from inputs
    new_mapping <- list()
    for (var in ph_info$placeholders) {
      input_id <- paste0("map_", var)
      if (!is.null(input[[input_id]]) && input[[input_id]] != "") {
        new_mapping[[var]] <- input[[input_id]]
      }
    }
    
    if (length(new_mapping) == 0) {
      showNotification("Please at leastmappingOneVariable", type = "warning")
      return()
    }
    
    # Validate mapping
    validation <- validate_mapping(
      ph_info$placeholders,
      new_mapping,
      names(values$data)
    )
    
    if (!validation$is_valid) {
      showNotification(
        paste("mappingValidationFailed:", validation$message),
        type = "error"
      )
      return()
    }
    
    # Apply mapping to model
    original_model <- input$model_editor
    mapped_model <- apply_mapping(original_model, new_mapping)
    
    # Update editor
    updateTextAreaInput(session, "model_editor", value = mapped_model)
    
    # Store mapping
    values$variable_mapping <- new_mapping
    
    # Add to library
    template <- values$model_template
    if (is.null(template)) {
      template <- original_model
    }
    
    model_id <- model_lib$add_model(
      name = model_name,
      template = template,
      mapping = new_mapping,
      final_syntax = mapped_model
    )
    
    # Trigger UI update (force multiple times to ensure reactivity)
    values$model_library_trigger <- values$model_library_trigger + 1
    
    showNotification(
      sprintf("Model '%s' AppliedmappingAnd saved toModel Library", model_name),
      type = "message",
      duration = 3
    )
    
    # Clear model name input
    updateTextInput(session, "model_name_mapping", value = "")
    
    # Force another trigger to ensure UI updates
    isolate({
      values$model_library_trigger <- values$model_library_trigger + 1
    })
  })
  
  # Show mapped model preview
  observeEvent(input$preview_mapped_model, {
    if (is.null(values$variable_mapping) || length(values$variable_mapping) == 0) {
      showNotification("Please firstEnterrowsVariablemapping", type = "warning")
      return()
    }
    
    original_model <- values$model_template
    if (is.null(original_model)) {
      original_model <- input$model_editor
    }
    
    mapped_model <- apply_mapping(original_model, values$variable_mapping)
    
    showModal(modalDialog(
      title = "mappingModel preview after",
      fluidRow(
        column(
          width = 6,
          h5("Original template"),
          tags$pre(style = "max-height: 400px; overflow-y: auto;", original_model)
        ),
        column(
          width = 6,
          h5("mappingModel after"),
          tags$pre(style = "max-height: 400px; overflow-y: auto;", mapped_model)
        )
      ),
      footer = tagList(
        actionButton("apply_preview", "Apply toEditProcessor", class = "btn-success"),
        modalButton("Close")
      ),
      size = "l"
    ))
  })
  
  # Apply preview to editor
  observeEvent(input$apply_preview, {
    if (!is.null(values$variable_mapping)) {
      original_model <- values$model_template
      if (is.null(original_model)) {
        original_model <- input$model_editor
      }
      mapped_model <- apply_mapping(original_model, values$variable_mapping)
      updateTextAreaInput(session, "model_editor", value = mapped_model)
      removeModal()
      showNotification("AppliedMap toEditProcessor", type = "message")
    }
  })
  
  # ============================================================================
  # MODEL LIBRARY MANAGEMENT
  # ============================================================================
  
  # Model library UI
  output$model_library_ui <- renderUI({
    # Force reactivity with trigger
    values$model_library_trigger
    
    # Get current models
    current_models <- model_lib$get_all()
    
    generate_model_library_ui(model_lib)
  })
  
  # Remove model from library
  observe({
    models <- model_lib$get_all()
    lapply(names(models), function(model_id) {
      observeEvent(input[[paste0("remove_model_", model_id)]], {
        model_lib$remove_model(model_id)
        values$model_library_trigger <- values$model_library_trigger + 1
        showNotification("Model deleted", type = "message")
      })
    })
  })
  
  # Edit model from library
  observe({
    models <- model_lib$get_all()
    lapply(names(models), function(model_id) {
      observeEvent(input[[paste0("edit_model_", model_id)]], {
        model <- models[[model_id]]
        if (!is.null(model)) {
          # Load model into editor
          updateTextAreaInput(session, "model_editor", value = model$template)
          values$model_template <- model$template
          values$variable_mapping <- model$mapping
          
          # Update model name
          updateTextInput(session, "model_name", value = model$name)
          
          # Switch to manual mode if needed
          updateRadioButtons(session, "model_design_mode", selected = "manual")
          
          showNotification(
            sprintf("LoadedModel '%s' ToEditProcessor", model$name),
            type = "message"
          )
        }
      })
    })
  })
  
  # Rename model from library
  observe({
    models <- model_lib$get_all()
    lapply(names(models), function(model_id) {
      observeEvent(input[[paste0("rename_model_", model_id)]], {
        model <- models[[model_id]]
        if (!is.null(model)) {
          showModal(modalDialog(
            title = "RenameModel",
            textInput(
              paste0("new_name_", model_id),
              "New name",
              value = model$name
            ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(
                paste0("confirm_rename_", model_id),
                "Confirm",
                class = "btn-primary"
              )
            )
          ))
        }
      })
    })
  })
  
  # Confirm rename
  observe({
    models <- model_lib$get_all()
    lapply(names(models), function(model_id) {
      observeEvent(input[[paste0("confirm_rename_", model_id)]], {
        new_name <- input[[paste0("new_name_", model_id)]]
        if (!is.null(new_name) && nchar(trimws(new_name)) > 0) {
          model <- models[[model_id]]
          model$name <- new_name
          model_lib$models[[model_id]] <- model
          values$model_library_trigger <- values$model_library_trigger + 1
          removeModal()
          showNotification(
            sprintf("Model hasRenameAs '%s'", new_name),
            type = "message"
          )
        } else {
          showNotification("Please enter a valid name", type = "warning")
        }
      })
    })
  })
  
  # Clear all models
  observeEvent(input$clear_all_models, {
    model_lib$clear_all()
    values$model_library_trigger <- values$model_library_trigger + 1
    showNotification("Model LibraryClearedEmpty", type = "message")
  })
  
  # ============================================================================
  # AUTO COMBINATION MODE
  # ============================================================================
  
  # Clear combination template
  observeEvent(input$clear_combo_template, {
    values$model_template <- NULL
    showNotification("Template cleared", type = "message")
  })
  
  # Load template for combination mode
  observeEvent(input$load_combo_template, {
    if (!is.null(input$combo_template_select)) {
      if (input$combo_template_select == "custom") {
        # Use custom template from editor
        template_content <- input$combo_custom_editor
        if (is.null(template_content) || nchar(trimws(template_content)) == 0) {
          showNotification("Please firstInEditProcessorEnter inCustomTemplate", type = "warning")
          return()
        }
      } else {
        # Get current engine type
        engine_type <- input$header_engine_select
        if (is.null(engine_type)) engine_type <- "r"
        
        # Load template based on engine type
        template_content <- load_template_by_engine(input$combo_template_select, engine_type)
      }
      
      if (template_content != "") {
        values$model_template <- template_content
        
        engine_label <- if (engine_type == "mplus") "Mplus" else "R/lavaan"
        showNotification(
          sprintf("Loaded %s Template (%sEngine)", input$combo_template_select, engine_label),
          type = "message"
        )
      }
    }
  })
  
  # Template preview for combination mode
  output$combo_template_preview <- renderText({
    if (!is.null(values$model_template)) {
      values$model_template
    } else {
      "Please firstLoad Template"
    }
  })
  
  # Multi-mapping UI
  output$multi_mapping_ui <- renderUI({
    if (is.null(values$model_template)) {
      return(div(
        class = "alert alert-info",
        "Please firstLoad Template"
      ))
    }
    
    ph_info <- extract_placeholders(values$model_template)
    available_vars <- if (!is.null(values$data)) names(values$data) else NULL
    
    if (ph_info$count == 0) {
      return(div(
        class = "alert alert-warning",
        "No in template {{Placeholder}}"
      ))
    }
    
    if (is.null(available_vars)) {
      return(div(
        class = "alert alert-warning",
        "Please upload data first"
      ))
    }
    
    generate_multi_mapping_ui(ph_info$placeholder_info, available_vars)
  })
  
  # Generate combinations
  observeEvent(input$generate_combinations, {
    if (is.null(values$model_template)) {
      showNotification("Please firstLoad Template", type = "warning")
      return()
    }
    
    if (is.null(values$data)) {
      showNotification("Please upload data first", type = "warning")
      return()
    }
    
    ph_info <- extract_placeholders(values$model_template)
    if (ph_info$count == 0) {
      showNotification("No in templatePlaceholder", type = "warning")
      return()
    }
    
    # Collect multi-mappings
    multi_mapping <- list()
    for (ph_name in ph_info$placeholders) {
      input_id <- paste0("multi_map_", ph_name)
      selected_vars <- input[[input_id]]
      
      if (is.null(selected_vars) || length(selected_vars) == 0) {
        showNotification(
          sprintf("Please for {{%s}} Select at least oneVariable", ph_name),
          type = "warning"
        )
        return()
      }
      
      multi_mapping[[ph_name]] <- selected_vars
    }
    
    # Generate combinations
    n_generated <- model_lib$generate_combinations(
      template = values$model_template,
      placeholder_info = ph_info$placeholder_info,
      multi_mapping = multi_mapping
    )
    
    # Trigger UI update
    values$model_library_trigger <- values$model_library_trigger + 1
    
    showNotification(
      sprintf("SuccessGenerate %d model combinations", n_generated),
      type = "message",
      duration = 5
    )
  })
  
  # Combination info
  output$combination_info <- renderUI({
    if (is.null(values$model_template)) {
      return(NULL)
    }
    
    ph_info <- extract_placeholders(values$model_template)
    if (ph_info$count == 0) {
      return(NULL)
    }
    
    # Calculate potential combinations
    counts <- sapply(ph_info$placeholders, function(ph_name) {
      input_id <- paste0("multi_map_", ph_name)
      selected <- input[[input_id]]
      if (is.null(selected)) 0 else length(selected)
    })
    
    total_combinations <- prod(counts[counts > 0])
    
    if (total_combinations > 0) {
      div(
        class = "alert alert-success",
        style = "margin-top: 10px;",
        icon("info-circle"),
        sprintf(" WillGenerate %d model combinations", total_combinations)
      )
    }
  })
  
  # ============================================================================
  # WORKFLOW TRACKING
  # ============================================================================
  
  # Update workflow when data is loaded
  observe({
    if (!is.null(values$data)) {
      workflow$set_data_loaded(TRUE)
    }
  })
  
  # Update workflow when template is loaded
  observe({
    if (!is.null(input$model_editor) && nchar(trimws(input$model_editor)) > 0) {
      workflow$set_template_loaded(TRUE)
    }
  })
  
  # Update workflow when variables are mapped
  observe({
    ph_info <- placeholders_info()
    if (ph_info$count > 0 && !is.null(values$variable_mapping)) {
      validation <- validate_mapping(
        ph_info$placeholders,
        values$variable_mapping,
        if (!is.null(values$data)) names(values$data) else NULL
      )
      if (validation$is_complete && validation$is_valid) {
        workflow$set_variables_mapped(TRUE)
      }
    }
  })
  
  # Workflow progress UI
  output$workflow_progress_ui <- renderUI({
    generate_workflow_ui(workflow)
  })
  
  # ============================================================================
  # BATCH PROCESSING
  # ============================================================================
  
  # Start batch analysis button
  observeEvent(input$start_batch, {
    # Validate prerequisites
    if (is.null(values$data)) {
      showNotification("Please upload data first", type = "warning")
      return()
    }
    
    # Check if there are models in the library
    models <- model_lib$get_all()
    
    if (length(models) > 0) {
      # Run all models from library
      showNotification(
        sprintf("Homebatch analysis %d models...", length(models)),
        type = "message"
      )
      
      # Set running state
      values$analysis_running <- TRUE
      values$analysis_start_time <- Sys.time()
      values$analysis_progress <- 0
      values$analysis_message <- "Homebatch analysis..."
      
      # Check current engine
      current_engine <- engine_mgr$current_engine
      engine_name <- if (!is.null(current_engine) && !is.null(current_engine$name)) {
        current_engine$name
      } else {
        "R"  # Default to R engine
      }
      
      # Progress callback
      update_progress <- function(prog, msg) {
        values$analysis_progress <- prog
        values$analysis_message <- msg
      }
      
      # Run batch analysis based on engine type
      if (!is.null(engine_name) && length(engine_name) > 0 && engine_name == "Mplus") {
        # Use Mplus batch processor
        tryCatch({
          # Check if MplusAutomation is available
          if (!requireNamespace("MplusAutomation", quietly = TRUE)) {
            stop("MplusAutomationpackageNot installed。Please run: install.packages('MplusAutomation')")
          }
          
          # Source the Mplus batch processor
          batch_processor_file <- "R/utils/mplus_batch_processor.R"
          if (file.exists(batch_processor_file)) {
            source(batch_processor_file)
          } else {
            stop(sprintf("Cannot findMplusBatch ProcessingProcessorFile: %s", batch_processor_file))
          }
          
          # Prepare models list
          mplus_models <- lapply(models, function(m) {
            list(name = m$name, syntax = m$final_syntax)
          })
          
          cat(sprintf("\n=== HomeMplusbatch analysis ===\n"))
          cat(sprintf("Number of models: %d\n", length(mplus_models)))
          cat(sprintf("DatarowsNumber: %d\n", nrow(values$data)))
          cat(sprintf("DatacolumnsNumber: %d\n\n", ncol(values$data)))
          
          # Run Mplus batch
          batch_results <- run_mplus_batch(
            data = values$data,
            models = mplus_models,
            output_dir = NULL,  # Will use temp directory
            progress_callback = update_progress
          )
          
          # Store results
          values$batch_results <- batch_results
          
          values$analysis_running <- FALSE
          values$analysis_progress <- 100
          values$analysis_message <- sprintf("Completed！Success: %d, Failed: %d", 
                                            batch_results$n_success, batch_results$n_failed)
          
          workflow$set_analysis_complete(TRUE)
          
          showNotification(
            sprintf("batch analysisCompleted！Success: %d, Failed: %d", 
                   batch_results$n_success, batch_results$n_failed),
            type = "message",
            duration = 5
          )
          
        }, error = function(e) {
          cat(sprintf("\n❌ batch analysisError: %s\n", e$message))
          cat(sprintf("ErrorStack:\n"))
          print(traceback())
          
          values$analysis_running <- FALSE
          values$analysis_message <- sprintf("Batch analysis failed: %s", e$message)
          showNotification(
            sprintf("Batch analysis failed: %s", e$message), 
            type = "error",
            duration = 10
          )
        })
        
      } else {
        # Use R/lavaan engine (original logic)
        all_results <- list()
        n_models <- length(models)
        
        for (i in seq_along(models)) {
          model <- models[[i]]
          
          # Update progress
          progress <- (i - 1) / n_models * 100
          values$analysis_progress <- progress
          values$analysis_message <- sprintf("Analysis model %d/%d: %s", i, n_models, model$name)
          
          tryCatch({
            # Progress callback for individual model
            update_model_progress <- function(prog, msg) {
              base_progress <- (i - 1) / n_models * 100
              model_progress <- prog / n_models
              values$analysis_progress <- base_progress + model_progress
              values$analysis_message <- sprintf("Model %d/%d - %s", i, n_models, msg)
            }
            
            # Run analysis
            result <- run_batch_analysis(
              data = values$data,
              model_syntax = model$final_syntax,
              engine_mgr = engine_mgr,
              progress_callback = update_model_progress
            )
            
            result$model_name <- model$name
            result$model_id <- model$id
            all_results[[model$id]] <- result
            
          }, error = function(e) {
            all_results[[model$id]] <<- list(
              success = FALSE,
              model_name = model$name,
              model_id = model$id,
              errors = c(e$message)
            )
          })
        }
        
        # Store all results
        values$batch_results <- list(
          success = TRUE,
          is_batch = TRUE,
          n_models = n_models,
          models = all_results,
          completed_at = Sys.time()
        )
        
        # Count successes and failures
        n_success <- sum(sapply(all_results, function(r) isTRUE(r$success)))
        n_failed <- n_models - n_success
        
        values$analysis_running <- FALSE
        values$analysis_progress <- 100
        values$analysis_message <- sprintf("Completed！Success: %d, Failed: %d", n_success, n_failed)
        
        workflow$set_analysis_complete(TRUE)
        
        showNotification(
          sprintf("batch analysisCompleted！Success: %d, Failed: %d", n_success, n_failed),
          type = "message",
          duration = 5
        )
      }
      
    } else {
      # No models in library, try to run single model from editor
      model_text <- input$model_editor
      if (is.null(model_text) || nchar(trimws(model_text)) == 0) {
        showNotification(
          "Please add models to the model library first，Or inEditProcessorEnter model in",
          type = "warning"
        )
        return()
      }
      
      # Check if variables are mapped
      ph_info <- placeholders_info()
      if (ph_info$count > 0) {
        if (is.null(values$variable_mapping) || length(values$variable_mapping) == 0) {
          showNotification("Please firstmappingVariable", type = "warning")
          return()
        }
        
        validation <- validate_mapping(
          ph_info$placeholders,
          values$variable_mapping,
          names(values$data)
        )
        
        if (!validation$is_complete || !validation$is_valid) {
          showNotification(
            paste("Variable mapping incompleteOr invalid:", validation$message),
            type = "warning"
          )
          return()
        }
      }
      
      # Set running state
      values$analysis_running <- TRUE
      values$analysis_start_time <- Sys.time()
      values$analysis_progress <- 0
      values$analysis_message <- "Start Analysis..."
      
      showNotification("Start AnalysisSinglemodels...", type = "message")
      
      # Run analysis
      tryCatch({
        # Progress callback
        update_progress <- function(progress, message) {
          values$analysis_progress <- progress
          values$analysis_message <- message
        }
        
        # Run batch analysis
        results <- run_batch_analysis(
          data = values$data,
          model_syntax = model_text,
          engine_mgr = engine_mgr,
          progress_callback = update_progress
        )
        
        # Store results
        values$batch_results <- results
        
        if (results$success) {
          workflow$set_analysis_complete(TRUE)
          showNotification("AnalysisCompleted！", type = "message", duration = 5)
        } else {
          showNotification(
            paste("AnalysisFailed:", paste(results$errors, collapse = "; ")),
            type = "error",
            duration = 10
          )
        }
        
      }, error = function(e) {
        showNotification(
          paste("Analysis error:", e$message),
          type = "error",
          duration = 10
        )
      }, finally = {
        values$analysis_running <- FALSE
        values$analysis_progress <- 100
      })
    }
  })
  
  # Pause batch button (placeholder)
  observeEvent(input$pause_batch, {
    showNotification("Pause function in development", type = "message")
  })
  
  # Stop batch button
  observeEvent(input$stop_batch, {
    values$analysis_running <- FALSE
    values$analysis_progress <- 0
    values$analysis_message <- "Stopped"
    showNotification("Analysis stopped", type = "message")
  })
  
  # ============================================================================
  # MPLUS SPECIFIC BATCH PROCESSING
  # ============================================================================
  
  # Browse output directory
  observeEvent(input$browse_output_dir, {
    if (.Platform$OS.type == "windows") {
      tryCatch({
        selected_dir <- choose.dir(default = getwd(), caption = "Select output folder")
        # Check if user selected a directory (not cancelled)
        if (!is.null(selected_dir) && !is.na(selected_dir) && nchar(selected_dir) > 0) {
          updateTextInput(session, "mplus_output_dir", value = selected_dir)
          showNotification("Folder selected", type = "message")
        } else if (!is.null(selected_dir)) {
          # User cancelled the dialog
          showNotification("Selection cancelled", type = "warning")
        }
      }, error = function(e) {
        showNotification(paste("Select folderFailed:", e$message), type = "error")
      })
    } else {
      showNotification(
        "FolderBrowseProcessorOnly supportsWindowsSystem，Please manually enter path",
        type = "warning"
      )
    }
  })
  
  # Browse run directory
  observeEvent(input$browse_run_dir, {
    if (.Platform$OS.type == "windows") {
      tryCatch({
        selected_dir <- choose.dir(default = getwd(), caption = "SelectinpFolder")
        # Check if user selected a directory (not cancelled)
        if (!is.null(selected_dir) && !is.na(selected_dir) && nchar(selected_dir) > 0) {
          updateTextInput(session, "mplus_run_dir", value = selected_dir)
          showNotification("Folder selected", type = "message")
        } else if (!is.null(selected_dir)) {
          # User cancelled the dialog
          showNotification("Selection cancelled", type = "warning")
        }
      }, error = function(e) {
        showNotification(paste("Select folderFailed:", e$message), type = "error")
      })
    } else {
      showNotification(
        "FolderBrowseProcessorOnly supportsWindowsSystem，Please manually enter path",
        type = "warning"
      )
    }
  })
  
  # Generate Mplus inp files
  observeEvent(input$generate_mplus_files, {
    # Validate prerequisites
    if (is.null(values$data)) {
      showNotification("Please upload data first", type = "warning")
      return()
    }
    
    models <- model_lib$get_all()
    if (length(models) == 0) {
      showNotification("Please add models to the model library first", type = "warning")
      return()
    }
    
    output_dir <- input$mplus_output_dir
    if (is.null(output_dir) || nchar(trimws(output_dir)) == 0) {
      output_dir <- getwd()
    }
    
    tryCatch({
      # Source the Mplus batch processor
      if (file.exists("R/utils/mplus_batch_processor.R")) {
        source("R/utils/mplus_batch_processor.R")
      }
      
      # Prepare models list
      mplus_models <- lapply(models, function(m) {
        list(name = m$name, syntax = m$final_syntax)
      })
      
      # Generate files
      result <- generate_mplus_files(
        data = values$data,
        models = mplus_models,
        output_dir = output_dir
      )
      
      # Update output
      output$mplus_generation_log <- renderText({
        log_text <- sprintf("=== FileGenerateSuccess ===\n\n")
        log_text <- paste0(log_text, sprintf("Output directory: %s\n\n", result$output_dir))
        log_text <- paste0(log_text, sprintf("data file: %s\n", basename(result$data_file)))
        log_text <- paste0(log_text, sprintf("input file: %d\n\n", length(result$inp_files)))
        log_text <- paste0(log_text, "Generatefiles:\n")
        for (f in result$inp_files) {
          log_text <- paste0(log_text, sprintf("  - %s\n", basename(f)))
        }
        log_text <- paste0(log_text, sprintf("\nNext step: In'Step2'InRunningThese files"))
        log_text
      })
      
      # Update run directory
      updateTextInput(session, "mplus_run_dir", value = output_dir)
      
      showNotification(
        sprintf("SuccessGenerate %d inpFile！", length(result$inp_files)),
        type = "message",
        duration = 5
      )
      
    }, error = function(e) {
      output$mplus_generation_log <- renderText({
        sprintf("❌ Error: %s", e$message)
      })
      showNotification(sprintf("GenerateFileFailed: %s", e$message), type = "error")
    })
  })
  
  # Run Mplus batch
  observeEvent(input$run_mplus_batch, {
    run_dir <- input$mplus_run_dir
    if (is.null(run_dir) || nchar(trimws(run_dir)) == 0) {
      showNotification("Please specifyinpFolder path", type = "warning")
      return()
    }
    
    if (!dir.exists(run_dir)) {
      showNotification("Specified folder does not exist", type = "error")
      return()
    }
    
    tryCatch({
      if (!requireNamespace("MplusAutomation", quietly = TRUE)) {
        stop("MplusAutomationpackageNot installed")
      }
      
      values$analysis_running <- TRUE
      values$analysis_progress <- 10
      values$analysis_message <- "Batch runMplusModel..."
      
      cat(sprintf("\n=== Batch runMplus ===\n"))
      cat(sprintf("Directory: %s\n\n", run_dir))
      
      # Run all inp files in directory
      MplusAutomation::runModels(run_dir, showOutput = FALSE, replaceOutfile = "always")
      
      values$analysis_progress <- 70
      values$analysis_message <- "Readresults..."
      
      # Read all results
      all_results <- MplusAutomation::readModels(run_dir)
      
      values$analysis_progress <- 100
      values$analysis_message <- "Completed！"
      values$analysis_running <- FALSE
      
      # Process and store results
      n_models <- if (is.list(all_results)) length(all_results) else 1
      
      # Convert Mplus results to standard format
      processed_results <- list()
      if (is.list(all_results)) {
        for (i in seq_along(all_results)) {
          result <- all_results[[i]]
          model_name <- if (!is.null(names(all_results)[i])) names(all_results)[i] else sprintf("Model_%d", i)
          
          processed_results[[model_name]] <- list(
            success = !is.null(result$summaries),
            model_name = model_name,
            fit_indices = if (!is.null(result$summaries)) {
              list(
                chisq = result$summaries$ChiSqM_Value,
                df = result$summaries$ChiSqM_DF,
                pvalue = result$summaries$ChiSqM_PValue,
                cfi = result$summaries$CFI,
                tli = result$summaries$TLI,
                rmsea = result$summaries$RMSEA_Estimate,
                srmr = result$summaries$SRMR,
                aic = result$summaries$AIC,
                bic = result$summaries$BIC
              )
            } else NULL,
            parameters = if (!is.null(result$parameters)) result$parameters$unstandardized else NULL
          )
        }
      }
      
      values$batch_results <- list(
        success = TRUE,
        is_batch = TRUE,
        n_models = n_models,
        models = processed_results,
        mplus_results = all_results,
        output_dir = run_dir,
        completed_at = Sys.time()
      )
      
      workflow$set_analysis_complete(TRUE)
      
      showNotification(
        sprintf("Batch runCompleted！Total processed %d models", n_models),
        type = "message",
        duration = 5
      )
      
    }, error = function(e) {
      values$analysis_running <- FALSE
      values$analysis_message <- sprintf("RunningFailed: %s", e$message)
      showNotification(sprintf("Batch runFailed: %s", e$message), type = "error")
    })
  })
  
  # Progress bar output
  output$batch_progress_bar <- renderUI({
    progress <- values$analysis_progress
    message <- values$analysis_message
    
    div(
      class = "progress",
      style = "height: 30px;",
      div(
        class = paste("progress-bar", 
                     if (values$analysis_running) "progress-bar-striped active" else "",
                     if (progress == 100 && !is.null(values$batch_results) && values$batch_results$success) "progress-bar-success" else "progress-bar-info"),
        role = "progressbar",
        style = sprintf("width: %d%%;", progress),
        sprintf("%d%% - %s", progress, message)
      )
    )
  })
  
  # Value boxes for batch status
  output$models_completed <- renderValueBox({
    if (!is.null(values$batch_results)) {
      if (isTRUE(values$batch_results$is_batch)) {
        n_success <- sum(sapply(values$batch_results$models, function(r) isTRUE(r$success)))
        valueBox(
          value = n_success,
          subtitle = "ModelCompleted",
          icon = icon("check-circle"),
          color = "green"
        )
      } else if (isTRUE(values$batch_results$success)) {
        valueBox(
          value = "1",
          subtitle = "ModelCompleted",
          icon = icon("check-circle"),
          color = "green"
        )
      } else {
        valueBox(
          value = "0",
          subtitle = "ModelCompleted",
          icon = icon("hourglass"),
          color = "yellow"
        )
      }
    } else {
      valueBox(
        value = "0",
        subtitle = "ModelCompleted",
        icon = icon("hourglass"),
        color = "yellow"
      )
    }
  })
  
  output$models_failed <- renderValueBox({
    if (!is.null(values$batch_results)) {
      if (isTRUE(values$batch_results$is_batch)) {
        n_success <- sum(sapply(values$batch_results$models, function(r) isTRUE(r$success)))
        n_failed <- values$batch_results$n_models - n_success
        valueBox(
          value = n_failed,
          subtitle = "ModelFailed",
          icon = if (n_failed > 0) icon("times-circle") else icon("check"),
          color = if (n_failed > 0) "red" else "green"
        )
      } else if (!isTRUE(values$batch_results$success)) {
        valueBox(
          value = "1",
          subtitle = "ModelFailed",
          icon = icon("times-circle"),
          color = "red"
        )
      } else {
        valueBox(
          value = "0",
          subtitle = "ModelFailed",
          icon = icon("check"),
          color = "green"
        )
      }
    } else {
      valueBox(
        value = "0",
        subtitle = "ModelFailed",
        icon = icon("check"),
        color = "green"
      )
    }
  })
  
  output$estimated_time <- renderValueBox({
    # Auto-invalidate every second when running
    if (values$analysis_running) {
      invalidateLater(1000, session)
    }
    
    if (values$analysis_running && !is.null(values$analysis_start_time)) {
      # Show elapsed time while running
      elapsed <- as.numeric(difftime(Sys.time(), values$analysis_start_time, units = "secs"))
      elapsed_str <- if (elapsed < 60) {
        sprintf("%.0fseconds", elapsed)
      } else {
        sprintf("%.1fminutes", elapsed / 60)
      }
      
      valueBox(
        value = elapsed_str,
        subtitle = "Time elapsed",
        icon = icon("clock"),
        color = "blue"
      )
    } else if (!is.null(values$batch_results) && !is.null(values$analysis_start_time)) {
      # Show total time after completion
      total_time <- as.numeric(difftime(
        values$batch_results$completed_at %||% Sys.time(),
        values$analysis_start_time,
        units = "secs"
      ))
      time_str <- if (total_time < 60) {
        sprintf("%.1fseconds", total_time)
      } else {
        sprintf("%.1fminutes", total_time / 60)
      }
      
      valueBox(
        value = time_str,
        subtitle = "Total time",
        icon = icon("check-circle"),
        color = "green"
      )
    } else {
      # Show number of pending models
      n_models <- model_lib$count()
      if (n_models > 0) {
        valueBox(
          value = sprintf("%d", n_models),
          subtitle = "Models to analyze",
          icon = icon("list"),
          color = "aqua"
        )
      } else {
        valueBox(
          value = "0",
          subtitle = "Models to analyze",
          icon = icon("info-circle"),
          color = "yellow"
        )
      }
    }
  })
  
  # Batch log output
  output$batch_log <- renderPrint({
    if (values$analysis_running) {
      cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), values$analysis_message))
      cat(sprintf("Progress: %d%%\n", values$analysis_progress))
    } else if (!is.null(values$batch_results)) {
      if (isTRUE(values$batch_results$is_batch)) {
        # Multiple models
        cat(sprintf("[%s] batch analysisCompleted\n", format(values$batch_results$completed_at, "%Y-%m-%d %H:%M:%S")))
        cat(sprintf("Total number of models: %d\n", values$batch_results$n_models))
        
        n_success <- sum(sapply(values$batch_results$models, function(r) isTRUE(r$success)))
        n_failed <- values$batch_results$n_models - n_success
        
        cat(sprintf("Success: %d\n", n_success))
        cat(sprintf("Failed: %d\n", n_failed))
        
        cat("\nModel details:\n")
        for (model_id in names(values$batch_results$models)) {
          result <- values$batch_results$models[[model_id]]
          status <- if (isTRUE(result$success)) "✓" else "✗"
          cat(sprintf("  %s %s\n", status, result$model_name))
          if (!isTRUE(result$success) && length(result$errors) > 0) {
            cat(sprintf("     Error: %s\n", paste(result$errors, collapse = "; ")))
          }
        }
      } else {
        # Single model
        cat(sprintf("[%s] AnalysisCompleted\n", format(values$batch_results$timestamp, "%Y-%m-%d %H:%M:%S")))
        cat(sprintf("Status: %s\n", if (isTRUE(values$batch_results$success)) "Success" else "Failed"))
        
        if (length(values$batch_results$warnings) > 0) {
          cat("\nWarning:\n")
          cat(paste("  -", values$batch_results$warnings, collapse = "\n"))
          cat("\n")
        }
        
        if (length(values$batch_results$errors) > 0) {
          cat("\nError:\n")
          cat(paste("  -", values$batch_results$errors, collapse = "\n"))
          cat("\n")
        }
      }
    } else {
      cat("WaitingStart Analysis...\n")
      cat("\n")
      cat("Tip:\n")
      cat("1. EnsureUpload Data\n")
      cat("2. InModel DesignAdd models to pageModel Library\n")
      cat("3. Or inEditProcessorEnter single inmodels\n")
      cat("4. Click'HomeBatch Processing'Button\n")
    }
  })
  
  # ============================================================================
  # RESULTS DISPLAY
  # ============================================================================
  
  # Model selector for results
  output$result_model_selector <- renderUI({
    if (is.null(values$batch_results)) {
      return(div(
        class = "alert alert-info",
        "Please firstRunningAnalysis"
      ))
    }
    
    if (isTRUE(values$batch_results$is_batch)) {
      models <- values$batch_results$models
      model_choices <- setNames(
        names(models),
        sapply(models, function(m) m$model_name)
      )
      
      fluidRow(
        column(
          width = 4,
          selectInput(
            "selected_result_model",
            "Select model to view",
            choices = c("All models" = "all", model_choices),
            selected = "all"
          )
        ),
        column(
          width = 8,
          div(
            style = "padding-top: 25px;",
            sprintf("Total %d models", length(models))
          )
        )
      )
    } else {
      div(
        class = "alert alert-success",
        icon("check-circle"),
        " SinglemodelsAnalysisresults"
      )
    }
  })
  
  # Results summary table
  output$results_summary <- renderDT({
    if (is.null(values$batch_results)) {
      return(datatable(
        data.frame(Message = "None availableresults，Please firstRunningAnalysis"),
        options = list(dom = 't')
      ))
    }
    
    # Check if batch results (multiple models)
    if (isTRUE(values$batch_results$is_batch)) {
      models <- values$batch_results$models
      selected_model <- input$selected_result_model
      
      # Filter models if specific model selected
      if (!is.null(selected_model) && selected_model != "all") {
        models <- models[selected_model]
      }
      
      # Create summary table
      summary_rows <- lapply(names(models), function(model_id) {
        result <- models[[model_id]]
        
        if (isTRUE(result$success) && !is.null(result$fit_indices)) {
          data.frame(
            Model = result$model_name,
            ChiSq = sprintf("%.2f", result$fit_indices$chisq),
            df = sprintf("%.0f", result$fit_indices$df),
            p = sprintf("%.3f", result$fit_indices$pvalue),
            CFI = sprintf("%.3f", result$fit_indices$cfi),
            TLI = sprintf("%.3f", result$fit_indices$tli),
            RMSEA = sprintf("%.3f", result$fit_indices$rmsea),
            SRMR = sprintf("%.3f", result$fit_indices$srmr),
            Status = "✓",
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            Model = result$model_name,
            ChiSq = "—",
            df = "—",
            p = "—",
            CFI = "—",
            TLI = "—",
            RMSEA = "—",
            SRMR = "—",
            Status = "✗",
            stringsAsFactors = FALSE
          )
        }
      })
      
      summary_df <- do.call(rbind, summary_rows)
      
      datatable(
        summary_df,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        caption = sprintf("Analysisresults (%d models)", nrow(summary_df)),
        rownames = FALSE
      )
    } else {
      # Single model result
      if (!isTRUE(values$batch_results$success)) {
        return(datatable(
          data.frame(Message = "AnalysisFailed，Please viewBatch ProcessingPage log"),
          options = list(dom = 't')
        ))
      }
      
      fit_df <- format_fit_indices(values$batch_results$fit_indices)
      datatable(
        fit_df,
        options = list(
          dom = 't',
          pageLength = 20
        ),
        caption = "Model fit indices",
        rownames = FALSE
      )
    }
  })
  
  # Parameter estimates table
  output$parameter_estimates <- renderDT({
    if (is.null(values$batch_results)) {
      return(datatable(
        data.frame(Message = "None availableresults"),
        options = list(dom = 't')
      ))
    }
    
    # Check if batch results
    if (isTRUE(values$batch_results$is_batch)) {
      selected_model <- input$selected_result_model
      
      if (is.null(selected_model) || selected_model == "all") {
        return(datatable(
          data.frame(Message = "Please select a specific model above to view detailsParameter Estimates"),
          options = list(dom = 't')
        ))
      }
      
      # Get selected model result
      result <- values$batch_results$models[[selected_model]]
      
      if (!isTRUE(result$success)) {
        return(datatable(
          data.frame(Message = sprintf("Model '%s' AnalysisFailed", result$model_name)),
          options = list(dom = 't')
        ))
      }
      
      if (is.null(result$parameters)) {
        return(datatable(
          data.frame(Message = "This model has noParameter Estimatesresults"),
          options = list(dom = 't')
        ))
      }
      
      params_df <- format_parameters(result$parameters)
      datatable(
        params_df,
        options = list(
          pageLength = 20,
          scrollX = TRUE
        ),
        caption = sprintf("Parameter Estimates - %s", result$model_name),
        rownames = FALSE
      )
    } else {
      # Single model result
      if (!isTRUE(values$batch_results$success)) {
        return(datatable(
          data.frame(Message = "AnalysisFailed"),
          options = list(dom = 't')
        ))
      }
      
      params_df <- format_parameters(values$batch_results$parameters)
      datatable(
        params_df,
        options = list(
          pageLength = 20,
          scrollX = TRUE
        ),
        caption = "Parameter Estimates",
        rownames = FALSE
      )
    }
  })
  
  # Pending models list
  output$pending_models_list <- renderUI({
    # Trigger update when models change or analysis completes
    input$add_to_library
    input$generate_combinations
    input$clear_all_models
    values$batch_results
    
    models <- model_lib$get_all()
    
    if (length(models) == 0) {
      return(div(
        class = "alert alert-info",
        style = "text-align: center; padding: 30px;",
        icon("info-circle", class = "fa-2x"),
        br(), br(),
        strong("Model LibraryAsEmpty"),
        br(),
        "Please inModel DesignAdd models to page"
      ))
    }
    
    # Create model list with status
    model_items <- lapply(names(models), function(model_id) {
      model <- models[[model_id]]
      
      # Determine status
      status_icon <- icon("clock", style = "color: #999;")
      status_text <- "Waiting"
      status_color <- "#f0f0f0"
      
      if (!is.null(values$batch_results) && isTRUE(values$batch_results$is_batch)) {
        # Try to match by model_id first, then by model name
        result <- NULL
        if (model_id %in% names(values$batch_results$models)) {
          result <- values$batch_results$models[[model_id]]
        } else {
          # Try to match by model name (for Mplus results)
          for (res_name in names(values$batch_results$models)) {
            res <- values$batch_results$models[[res_name]]
            if (!is.null(res$model_name) && res$model_name == model$name) {
              result <- res
              break
            }
            # Also try matching the base name without extension
            if (grepl(model$name, res_name, fixed = TRUE) || grepl(res_name, model$name, fixed = TRUE)) {
              result <- res
              break
            }
          }
        }
        
        if (!is.null(result)) {
          if (isTRUE(result$success)) {
            status_icon <- icon("check-circle", style = "color: #00a65a;")
            status_text <- "Success"
            status_color <- "#d4edda"
          } else {
            status_icon <- icon("times-circle", style = "color: #dd4b39;")
            status_text <- "Failed"
            status_color <- "#f8d7da"
          }
        }
      }
      
      div(
        style = sprintf("padding: 12px; margin-bottom: 8px; border-left: 4px solid %s; background-color: %s; border-radius: 4px;",
                       if (status_text == "Success") "#00a65a" else if (status_text == "Failed") "#dd4b39" else "#999",
                       status_color),
        fluidRow(
          column(
            width = 2,
            style = "text-align: center; font-size: 20px;",
            status_icon
          ),
          column(
            width = 10,
            div(
              style = "font-weight: bold; font-size: 14px;",
              model$name
            ),
            div(
              style = "font-size: 11px; color: #666; margin-top: 4px;",
              sprintf("Created Time: %s", format(model$created_at, "%m-%d %H:%M"))
            ),
            div(
              style = "font-size: 11px; margin-top: 2px;",
              tags$span(
                style = sprintf("color: %s; font-weight: bold;",
                              if (status_text == "Success") "#00a65a" else if (status_text == "Failed") "#dd4b39" else "#999"),
                status_text
              )
            )
          )
        )
      )
    })
    
    tagList(
      div(
        style = "margin-bottom: 15px; padding: 10px; background-color: #f4f4f4; border-radius: 4px;",
        strong(sprintf("Total %d models", length(models))),
        if (!is.null(values$batch_results) && isTRUE(values$batch_results$is_batch)) {
          n_success <- sum(sapply(values$batch_results$models, function(r) isTRUE(r$success)))
          n_failed <- length(values$batch_results$models) - n_success
          tagList(
            br(),
            sprintf("Success: %d | Failed: %d", n_success, n_failed)
          )
        }
      ),
      div(
        style = "max-height: 500px; overflow-y: auto;",
        model_items
      ),
      hr(),
      div(
        style = "font-size: 11px; color: #666; text-align: center;",
        icon("clock", style = "color: #999;"), " Waiting　",
        icon("check-circle", style = "color: #00a65a;"), " Success　",
        icon("times-circle", style = "color: #dd4b39;"), " Failed"
      )
    )
  })
  
  # ============================================================================
  # VISUALIZATION
  # ============================================================================
  
  # Model selector for visualization
  output$viz_model_selector <- renderUI({
    if (is.null(values$batch_results)) {
      return(NULL)
    }
    
    if (isTRUE(values$batch_results$is_batch)) {
      models <- values$batch_results$models
      model_choices <- setNames(
        names(models),
        sapply(models, function(m) m$model_name)
      )
      
      selectInput(
        "selected_viz_model",
        "Select Model（Path Diagram）",
        choices = c("All models" = "all", model_choices),
        selected = "all"
      )
    } else {
      div(
        style = "padding: 10px; background: #d9edf7; border-radius: 4px;",
        icon("info-circle"),
        " Singlemodels"
      )
    }
  })
  
  # Generate plot
  observeEvent(input$generate_plot, {
    if (is.null(values$batch_results)) {
      showNotification("Please firstRunningAnalysis", type = "warning")
      return()
    }
    
    showNotification("In progressGenerateChart...", type = "message")
  })
  
  # Visualization plot output
  output$visualization_plot <- renderPlot({
    # Trigger on button click
    input$generate_plot
    
    if (is.null(values$batch_results)) {
      plot(1, type = "n", xlab = "", ylab = "", main = "Please firstRunningAnalysis", axes = FALSE)
      text(1, 1, "Please firstRun Batch Analysis\nThen click'GenerateChart'Button", cex = 1.5, col = "gray")
      return()
    }
    
    plot_type <- isolate(input$plot_type)
    
    if (plot_type == "fit_compare") {
      # Fit indices comparison plot
      if (isTRUE(values$batch_results$is_batch)) {
        models <- values$batch_results$models
        
        # Extract fit indices
        fit_data <- lapply(names(models), function(model_id) {
          result <- models[[model_id]]
          if (isTRUE(result$success) && !is.null(result$fit_indices)) {
            data.frame(
              Model = result$model_name,
              CFI = result$fit_indices$cfi,
              TLI = result$fit_indices$tli,
              RMSEA = result$fit_indices$rmsea,
              SRMR = result$fit_indices$srmr,
              stringsAsFactors = FALSE
            )
          } else {
            NULL
          }
        })
        
        fit_df <- do.call(rbind, fit_data[!sapply(fit_data, is.null)])
        
        if (is.null(fit_df) || nrow(fit_df) == 0) {
          plot(1, type = "n", xlab = "", ylab = "", main = "NoSuccessmodelsresults", axes = FALSE)
          text(1, 1, "AllModel execution failed\nPlease checkModel SyntaxAnd data", cex = 1.5, col = "red")
          return()
        }
        
        # Create comparison plot
        par(mfrow = c(2, 2), mar = c(10, 4, 3, 2))
        
        # CFI
        barplot(fit_df$CFI, names.arg = fit_df$Model, las = 2, 
                main = "CFI (Comparative Fit Index)", 
                ylab = "CFI", col = "steelblue", ylim = c(0, 1))
        abline(h = 0.95, col = "green", lty = 2, lwd = 2)
        abline(h = 0.90, col = "orange", lty = 2, lwd = 2)
        
        # TLI
        barplot(fit_df$TLI, names.arg = fit_df$Model, las = 2,
                main = "TLI (Tucker-Lewis Index)",
                ylab = "TLI", col = "coral", ylim = c(0, 1))
        abline(h = 0.95, col = "green", lty = 2, lwd = 2)
        abline(h = 0.90, col = "orange", lty = 2, lwd = 2)
        
        # RMSEA
        barplot(fit_df$RMSEA, names.arg = fit_df$Model, las = 2,
                main = "RMSEA (Root Mean Square Error)",
                ylab = "RMSEA", col = "lightgreen", ylim = c(0, max(fit_df$RMSEA) * 1.2))
        abline(h = 0.05, col = "green", lty = 2, lwd = 2)
        abline(h = 0.08, col = "orange", lty = 2, lwd = 2)
        
        # SRMR
        barplot(fit_df$SRMR, names.arg = fit_df$Model, las = 2,
                main = "SRMR (Standardized Root Mean Square Residual)",
                ylab = "SRMR", col = "plum", ylim = c(0, max(fit_df$SRMR) * 1.2))
        abline(h = 0.05, col = "green", lty = 2, lwd = 2)
        abline(h = 0.08, col = "orange", lty = 2, lwd = 2)
        
      } else {
        plot(1, type = "n", xlab = "", ylab = "", main = "Fit Indices ComparisonNeed multiplemodels", axes = FALSE)
        text(1, 1, "PleaseRun Batch Analysis（Multiplemodels）\nTo enterrowsFit Indices Comparison", cex = 1.5, col = "gray")
      }
    } else if (plot_type == "success_rate") {
      # Model success rate pie chart
      if (isTRUE(values$batch_results$is_batch)) {
        models <- values$batch_results$models
        n_success <- sum(sapply(models, function(r) isTRUE(r$success)))
        n_failed <- length(models) - n_success
        
        if (n_success + n_failed > 0) {
          pie_data <- c(n_success, n_failed)
          pie_labels <- c(
            sprintf("Success\n%d (%.1f%%)", n_success, n_success/(n_success+n_failed)*100),
            sprintf("Failed\n%d (%.1f%%)", n_failed, n_failed/(n_success+n_failed)*100)
          )
          pie_colors <- c("#00a65a", "#dd4b39")
          
          pie(pie_data, labels = pie_labels, col = pie_colors,
              main = sprintf("Model analysisSuccessRate\n(Total %d models)", length(models)),
              cex = 1.2, cex.main = 1.5)
          
          # Add legend
          legend("topright", 
                 legend = c("Success", "Failed"),
                 fill = pie_colors,
                 cex = 1.1)
        } else {
          plot(1, type = "n", xlab = "", ylab = "", main = "No analysisresults", axes = FALSE)
        }
      } else {
        plot(1, type = "n", xlab = "", ylab = "", main = "Needbatch analysisresults", axes = FALSE)
        text(1, 1, "PleaseRun Batch Analysis（Multiplemodels）\nTo displaySuccessRate statistics", cex = 1.5, col = "gray")
      }
    } else if (plot_type == "param_dist") {
      # Parameter estimates distribution
      if (isTRUE(values$batch_results$is_batch)) {
        models <- values$batch_results$models
        
        # Extract CFI and RMSEA values
        cfi_values <- sapply(models, function(r) {
          if (isTRUE(r$success) && !is.null(r$fit_indices)) {
            r$fit_indices$cfi
          } else {
            NA
          }
        })
        cfi_values <- cfi_values[!is.na(cfi_values)]
        
        rmsea_values <- sapply(models, function(r) {
          if (isTRUE(r$success) && !is.null(r$fit_indices)) {
            r$fit_indices$rmsea
          } else {
            NA
          }
        })
        rmsea_values <- rmsea_values[!is.na(rmsea_values)]
        
        if (length(cfi_values) > 1 || length(rmsea_values) > 1) {
          par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
          
          # CFI histogram
          if (length(cfi_values) > 1) {
            hist(cfi_values, breaks = 10, col = "lightblue", border = "white",
                 main = "CFI Distribution (Fit indices)", xlab = "CFI", ylab = "Frequency",
                 xlim = c(0, 1))
            abline(v = 0.95, col = "green", lty = 2, lwd = 2)
            abline(v = 0.90, col = "orange", lty = 2, lwd = 2)
            legend("topleft", 
                   legend = c("Excellent (≥0.95)", "Acceptable (≥0.90)"),
                   col = c("green", "orange"),
                   lty = 2, lwd = 2, cex = 0.8)
          }
          
          # CFI boxplot
          if (length(cfi_values) > 1) {
            boxplot(cfi_values, col = "lightblue", 
                    main = "CFI Boxplot (Fit indices)", ylab = "CFI",
                    ylim = c(0, 1))
            abline(h = 0.95, col = "green", lty = 2, lwd = 2)
            abline(h = 0.90, col = "orange", lty = 2, lwd = 2)
          }
          
          # RMSEA histogram
          if (length(rmsea_values) > 1) {
            hist(rmsea_values, breaks = 10, col = "lightcoral", border = "white",
                 main = "RMSEA Distribution (Fit indices)", xlab = "RMSEA", ylab = "Frequency")
            abline(v = 0.05, col = "green", lty = 2, lwd = 2)
            abline(v = 0.08, col = "orange", lty = 2, lwd = 2)
            legend("topright",
                   legend = c("Excellent (≤0.05)", "Acceptable (≤0.08)"),
                   col = c("green", "orange"),
                   lty = 2, lwd = 2, cex = 0.8)
          }
          
          # RMSEA boxplot
          if (length(rmsea_values) > 1) {
            boxplot(rmsea_values, col = "lightcoral",
                    main = "RMSEA Boxplot (Fit indices)", ylab = "RMSEA")
            abline(h = 0.05, col = "green", lty = 2, lwd = 2)
            abline(h = 0.08, col = "orange", lty = 2, lwd = 2)
          }
        } else {
          plot(1, type = "n", xlab = "", ylab = "", main = "Insufficient data", axes = FALSE)
          text(1, 1, "Need at least2Successmodels\nTo display distribution plot", cex = 1.5, col = "gray")
        }
      } else {
        plot(1, type = "n", xlab = "", ylab = "", main = "Needbatch analysisresults", axes = FALSE)
        text(1, 1, "PleaseRun Batch Analysis", cex = 1.5, col = "gray")
      }
    } else if (plot_type == "chisq_compare") {
      # Chi-square comparison
      if (isTRUE(values$batch_results$is_batch)) {
        models <- values$batch_results$models
        
        chisq_data <- lapply(names(models), function(model_id) {
          result <- models[[model_id]]
          if (isTRUE(result$success) && !is.null(result$fit_indices)) {
            data.frame(
              Model = result$model_name,
              ChiSq = result$fit_indices$chisq,
              df = result$fit_indices$df,
              p_value = result$fit_indices$pvalue,
              stringsAsFactors = FALSE
            )
          } else {
            NULL
          }
        })
        
        chisq_df <- do.call(rbind, chisq_data[!sapply(chisq_data, is.null)])
        
        if (!is.null(chisq_df) && nrow(chisq_df) > 0) {
          # Remove infinite values
          chisq_df$ChiSq[is.infinite(chisq_df$ChiSq)] <- NA
          chisq_df$p_value[is.infinite(chisq_df$p_value)] <- NA
          
          # Check if we have valid data
          if (all(is.na(chisq_df$ChiSq)) && all(is.na(chisq_df$p_value))) {
            plot(1, type = "n", xlab = "", ylab = "", main = "Invalid data", axes = FALSE)
            text(1, 1, "Chi-square values of all models orpInvalid value\n（MaypackageContains infinite values）", cex = 1.5, col = "gray")
            return()
          }
          
          par(mfrow = c(1, 2), mar = c(10, 4, 3, 2))
          
          # Chi-square values
          valid_chisq <- !is.na(chisq_df$ChiSq)
          if (any(valid_chisq)) {
            chisq_max <- max(chisq_df$ChiSq[valid_chisq], na.rm = TRUE)
            barplot(chisq_df$ChiSq, names.arg = chisq_df$Model, las = 2,
                    main = "Chi-Square Comparison", ylab = "χ²", col = "lightsteelblue",
                    cex.names = 0.7, ylim = c(0, chisq_max * 1.2))
          } else {
            plot(1, type = "n", xlab = "", ylab = "", main = "Invalid chi-square value", axes = FALSE)
          }
          
          # P-values
          valid_p <- !is.na(chisq_df$p_value)
          if (any(valid_p)) {
            p_max <- max(chisq_df$p_value[valid_p], na.rm = TRUE)
            barplot(chisq_df$p_value, names.arg = chisq_df$Model, las = 2,
                    main = "pValue comparison", ylab = "pValue", col = "lightyellow",
                    cex.names = 0.7, ylim = c(0, min(p_max * 1.2, 1)))
            abline(h = 0.05, col = "red", lty = 2, lwd = 2)
            legend("topright", 
                   legend = "Significance level (p=0.05)",
                   col = "red", lty = 2, lwd = 2, cex = 0.8)
          } else {
            plot(1, type = "n", xlab = "", ylab = "", main = "pInvalid value", axes = FALSE)
          }
        } else {
          plot(1, type = "n", xlab = "", ylab = "", main = "NoSuccessmodelsresults", axes = FALSE)
        }
      } else {
        plot(1, type = "n", xlab = "", ylab = "", main = "Needbatch analysisresults", axes = FALSE)
        text(1, 1, "PleaseRun Batch Analysis", cex = 1.5, col = "gray")
      }
    } else if (plot_type == "path") {
      # Path diagram using custom drawing functions
      tryCatch({
        # Load path diagram utilities
        if (file.exists("R/utils/path_diagram.R")) {
          source("R/utils/path_diagram.R")
        }
        
        # Get the model to plot
        selected_model <- input$selected_viz_model
        model_name <- "Model"
        
        if (isTRUE(values$batch_results$is_batch)) {
          if (is.null(selected_model) || selected_model == "all") {
            plot(1, type = "n", xlab = "", ylab = "", main = "Please select onemodels", axes = FALSE)
            text(1, 1, "Path DiagramNeed to select singlemodels\nPlease select one abovemodels", cex = 1.2, col = "gray")
            return()
          }
          
          result <- values$batch_results$models[[selected_model]]
          if (!isTRUE(result$success)) {
            plot(1, type = "n", xlab = "", ylab = "", main = "Model analysisFailed", axes = FALSE)
            text(1, 1, sprintf("Model '%s' AnalysisFailed\nUnable toGeneratePath Diagram", result$model_name), cex = 1.2, col = "gray")
            return()
          }
          
          fit_obj <- result$fit_object
          model_name <- result$model_name
        } else {
          if (!isTRUE(values$batch_results$success)) {
            plot(1, type = "n", xlab = "", ylab = "", main = "Model analysisFailed", axes = FALSE)
            text(1, 1, "Model analysisFailed\nUnable toGeneratePath Diagram", cex = 1.2, col = "gray")
            return()
          }
          
          fit_obj <- values$batch_results$fit_object
        }
        
        # Check if we have lavaan fit object
        # Mplus results don't have fit_object, show message
        if (is.null(fit_obj) || !is.null(values$batch_results$mplus_results)) {
          plot(1, type = "n", xlab = "", ylab = "", main = "Path DiagramNot available", axes = FALSE)
          text(1, 1, "MplusEngine does not support yetPath DiagramGenerate\n\nRecommend：\n1. UseR/lavaanEngineGeneratePath Diagram\n2. OrUseMplusSoftwarePLOTFunction\n3. Or draw manuallyPath Diagram", 
               cex = 1, col = "gray40")
          return()
        }
        
        if (is.null(fit_obj)) {
          plot(1, type = "n", xlab = "", ylab = "", main = "Cannot get model object", axes = FALSE)
          text(1, 1, "lavaanFit object not available\nPlease reRunningAnalysis", cex = 1.2, col = "gray")
          return()
        }
        
        # Generate path diagram using auto-detection
        if (exists("draw_auto_diagram")) {
          draw_auto_diagram(fit_obj, model_name)
        } else {
          # Fallback to semPlot if available
          if (requireNamespace("semPlot", quietly = TRUE)) {
            semPlot::semPaths(
              fit_obj,
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
              mar = c(3, 3, 3, 3)
            )
            title(main = model_name, line = 1, cex.main = 1.5)
          } else {
            plot(1, type = "n", xlab = "", ylab = "", main = "Need to install semPlot package", axes = FALSE)
            text(1, 1, "Please run: install.packages('semPlot')", cex = 1.2, col = "gray")
          }
        }
        
      }, error = function(e) {
        plot(1, type = "n", xlab = "", ylab = "", main = "Path DiagramGenerateFailed", axes = FALSE)
        text(1, 1, sprintf("Error：%s\n\nPossible reasons：\n1. Model object incomplete\n2. Model structure not supported\n3. semPlotpackageNot installed", e$message), cex = 1, col = "red")
      })
    }
  })
  
  # Download plot
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("sem_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      png(file, width = input$plot_width * 100, height = input$plot_height * 100, res = 100)
      
      # Regenerate the plot
      if (!is.null(values$batch_results) && isTRUE(values$batch_results$is_batch)) {
        models <- values$batch_results$models
        fit_data <- lapply(names(models), function(model_id) {
          result <- models[[model_id]]
          if (isTRUE(result$success) && !is.null(result$fit_indices)) {
            data.frame(
              Model = result$model_name,
              CFI = result$fit_indices$cfi,
              TLI = result$fit_indices$tli,
              RMSEA = result$fit_indices$rmsea,
              SRMR = result$fit_indices$srmr,
              stringsAsFactors = FALSE
            )
          } else {
            NULL
          }
        })
        
        fit_df <- do.call(rbind, fit_data[!sapply(fit_data, is.null)])
        
        par(mfrow = c(2, 2), mar = c(10, 4, 3, 2))
        
        barplot(fit_df$CFI, names.arg = fit_df$Model, las = 2, 
                main = "CFI", ylab = "CFI", col = "steelblue", ylim = c(0, 1))
        abline(h = 0.95, col = "green", lty = 2, lwd = 2)
        
        barplot(fit_df$TLI, names.arg = fit_df$Model, las = 2,
                main = "TLI", ylab = "TLI", col = "coral", ylim = c(0, 1))
        abline(h = 0.95, col = "green", lty = 2, lwd = 2)
        
        barplot(fit_df$RMSEA, names.arg = fit_df$Model, las = 2,
                main = "RMSEA", ylab = "RMSEA", col = "lightgreen")
        abline(h = 0.05, col = "green", lty = 2, lwd = 2)
        
        barplot(fit_df$SRMR, names.arg = fit_df$Model, las = 2,
                main = "SRMR", ylab = "SRMR", col = "plum")
        abline(h = 0.05, col = "green", lty = 2, lwd = 2)
      }
      
      dev.off()
    }
  )
  
  # ============================================================================
  # Settings Page
  # ============================================================================
  
  # R engine status detail
  output$r_engine_status <- renderPrint({
    if (!is.null(engine_mgr)) {
      status <- engine_mgr$get_status()
      if ("r" %in% names(status$engine_details)) {
        eng <- status$engine_details[["r"]]
        cat(sprintf("Engine name: %s\n", eng$name))
        cat(sprintf("Type: %s\n", eng$type))
        cat(sprintf("Version: %s\n", eng$version))
        cat(sprintf("Status: %s\n", if (eng$available) "✓ Available" else "✗ Not available"))
        if (eng$available) {
          cat("\nFeatures:\n")
          cat("  • CompleteSEMModeling function\n")
          cat("  • Rich fit indices\n")
          cat("  • FlexibleModel Syntax\n")
          cat("  • Free and open source\n")
        }
      } else {
        cat("R/lavaan Engine information not available\n")
      }
    } else {
      cat("Engine ManagementProcessorNot initialized\n")
    }
  })
  
  # Mplus engine status detail
  output$mplus_engine_status <- renderPrint({
    if (!is.null(engine_mgr)) {
      status <- engine_mgr$get_status()
      if ("mplus" %in% names(status$engine_details)) {
        eng <- status$engine_details[["mplus"]]
        cat(sprintf("Engine name: %s\n", eng$name))
        cat(sprintf("Type: %s\n", eng$type))
        cat(sprintf("Version: %s\n", eng$version))
        cat(sprintf("Status: %s\n", if (eng$available) "✓ Available" else "✗ Not available"))
        if (eng$available) {
          cat("\nFeatures:\n")
          cat("  • Supports complexModel\n")
          cat("  • ClassificationVariableAnalysis\n")
          cat("  • Multilevel model\n")
          cat("  • Requires commercial license\n")
        } else {
          cat("\nTip: Need to installMplusSoftware\n")
        }
      } else {
        cat("Mplus Engine information not available\n")
      }
    } else {
      cat("Engine ManagementProcessorNot initialized\n")
    }
  })
  
  # Engine selection (settings -> header), prevent ping-pong updates
  observeEvent(input$default_engine, ignoreInit = TRUE, {
    if (!is.null(engine_mgr)) {
      tryCatch({
        engine_mgr$switch_engine(input$default_engine)
        values$current_engine <- engine_mgr$current_engine$name
        
        # Update header selector only if different
        if (!identical(input$header_engine_select, input$default_engine)) {
          updateSelectInput(session, "header_engine_select", selected = input$default_engine)
        }
        
        engine_label <- if (input$default_engine == "r") "R/lavaan" else "Mplus"
        showNotification(
          sprintf("Switched to %s Engine", engine_label), 
          type = "message"
        )
      }, error = function(e) {
        showNotification(
          sprintf("Engine switchFailed: %s\n\nPlease ensure corresponding software is installedpackage", e$message), 
          type = "error",
          duration = 5
        )
      })
    }
  })
  
  # Header engine selection (header -> settings), prevent ping-pong updates
  observeEvent(input$header_engine_select, ignoreInit = TRUE, {
    if (!is.null(engine_mgr)) {
      tryCatch({
        engine_mgr$switch_engine(input$header_engine_select)
        values$current_engine <- engine_mgr$current_engine$name
        # Update settings page selector only if different
        if (!identical(input$default_engine, input$header_engine_select)) {
          updateRadioButtons(session, "default_engine", selected = input$header_engine_select)
        }
        
        engine_label <- if (input$header_engine_select == "r") "R/lavaan" else "Mplus"
        showNotification(
          sprintf("Switched to %s Engine", engine_label),
          type = "message"
        )
      }, error = function(e) {
        showNotification(
          sprintf("Engine switchFailed: %s", e$message),
          type = "error"
        )
      })
    }
  })
  
  # Theme selection
  observeEvent(input$ui_theme, {
    showNotification(
      "ThemeSwitching requires reloading application。Please save current work and refresh page。",
      type = "warning",
      duration = 5
    )
  })
  
  # Language selection
  observeEvent(input$ui_language, {
    showNotification(
      "LanguageSwitch function will be implemented in future versions。",
      type = "message",
      duration = 3
    )
  })
  
  # Save settings
  observeEvent(input$save_settings, {
    # Save engine preference
    if (!is.null(input$default_engine)) {
      # In a real implementation, save to config file
      showNotification(
        sprintf("Settings saved：Engine = %s", input$default_engine),
        type = "message"
      )
    }
  })
  
  # Reset settings
  observeEvent(input$reset_settings, {
    updateRadioButtons(session, "default_engine", selected = "r")
    updateSelectInput(session, "ui_theme", selected = "flatly")
    updateSelectInput(session, "ui_language", selected = "zh-CN")
    showNotification("SettingsReset to default values", type = "message")
  })
  
  # ============================================================================
  # CLEANUP
  # ============================================================================
  
  # Clean up on session end
  session$onSessionEnded(function() {
    log_info("User session ended")
    if (!is.null(engine_mgr)) {
      tryCatch({
        engine_mgr$cleanup_all()
      }, error = function(e) {
        log_warning(sprintf("Engine cleanup failed: %s", e$message))
      })
    }
    if (exists("clean_temp_files", mode = "function")) {
      clean_temp_files(max_age_hours = 1)
    }
  })
}

# Export server function
server <- create_main_server

