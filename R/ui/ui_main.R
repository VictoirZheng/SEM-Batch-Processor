# Main UI Definition
# Enhanced responsive UI with wizard-style workflow

#' Create main UI
#' @return Shiny UI object
create_main_ui <- function() {
  # Load configuration
  config <- if (exists("get_config", mode = "function")) {
    get_config("ui")
  } else {
    list(theme = "flatly", max_file_size_mb = 100)
  }
  
  ui <- dashboardPage(
    skin = "blue",
    
    # Header
    dashboardHeader(
      title = "SEM Batch Processor v2.0",
      titleWidth = 250,
      
      # Notifications
      dropdownMenu(
        type = "notifications",
        icon = icon("bell"),
        badgeStatus = "warning",
        headerText = "System Notifications",
        notificationItem(
          text = "Welcome to the Enhanced SEM Batch Processor",
          icon = icon("info-circle"),
          status = "info"
        )
      ),
      
      # Engine selector in header
      tags$li(
        class = "dropdown",
        style = "padding: 10px;",
        selectInput(
          "header_engine_select",
          label = NULL,
          choices = c("R/lavaan (Recommended)" = "r", "Mplus (Alternative)" = "mplus"),
          selected = "r",
          width = "180px"
        )
      )
    ),
    
    # Sidebar
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "sidebar_menu",
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Data Management", tabName = "data", icon = icon("database"),
                 badgeLabel = "1", badgeColor = "green"),
        menuItem("Model Design", tabName = "model", icon = icon("project-diagram"),
                 badgeLabel = "2", badgeColor = "green"),
        menuItem("Batch Processing", tabName = "batch", icon = icon("tasks"),
                 badgeLabel = "3", badgeColor = "green"),
        menuItem("Results Analysis", tabName = "results", icon = icon("chart-bar"),
                 badgeLabel = "4", badgeColor = "green"),
        menuItem("Visualization", tabName = "visualization", icon = icon("image"),
                 badgeLabel = "5", badgeColor = "green"),
        menuItem("Settings", tabName = "settings", icon = icon("cog")),
        menuItem("Help", tabName = "help", icon = icon("question-circle"))
      ),
      
      # Progress indicator
      div(
        style = "padding: 15px;",
        h5("Workflow Progress"),
        tags$div(
          class = "progress",
          tags$div(
            class = "progress-bar progress-bar-info",
            role = "progressbar",
            style = "width: 0%",
            id = "workflow_progress",
            "0%"
          )
        )
      )
    ),
    
    # Body
    dashboardBody(
      # Custom CSS and JavaScript
      tags$head(
        tags$script(src = "custom.js"),
        tags$style(HTML("
          .content-wrapper { background-color: #f4f6f9; }
          .box { border-top: 3px solid #3c8dbc; }
          .wizard-step { padding: 20px; margin-bottom: 20px; }
          .wizard-step.active { background-color: #e8f4f8; border-left: 4px solid #3c8dbc; }
          .wizard-step.completed { background-color: #f0f9ff; border-left: 4px solid #00a65a; }
          .step-number { 
            display: inline-block; 
            width: 30px; 
            height: 30px; 
            line-height: 30px; 
            text-align: center; 
            border-radius: 50%; 
            background-color: #3c8dbc; 
            color: white; 
            margin-right: 10px; 
          }
          .drag-drop-zone { 
            border: 2px dashed #ccc; 
            border-radius: 5px; 
            padding: 40px; 
            text-align: center; 
            background-color: #fafafa; 
          }
          .drag-drop-zone:hover { 
            border-color: #3c8dbc; 
            background-color: #f0f9ff; 
          }
        "))
      ),
      
      # Use shinyjs
      useShinyjs(),
      
      # Tab items
      tabItems(
        # Home tab
        tabItem(
          tabName = "home",
          create_home_tab()
        ),
        
        # Data management tab
        tabItem(
          tabName = "data",
          create_data_tab()
        ),
        
        # Model design tab
        tabItem(
          tabName = "model",
          create_model_tab()
        ),
        
        # Batch processing tab
        tabItem(
          tabName = "batch",
          create_batch_tab()
        ),
        
        # Results analysis tab
        tabItem(
          tabName = "results",
          create_results_tab()
        ),
        
        # Visualization tab
        tabItem(
          tabName = "visualization",
          create_visualization_tab()
        ),
        
        # Settings tab
        tabItem(
          tabName = "settings",
          create_settings_tab()
        ),
        
        # Help tab
        tabItem(
          tabName = "help",
          create_help_tab()
        )
      )
    )
  )
  
  return(ui)
}

#' Create home tab
create_home_tab <- function() {
  fluidRow(
    column(
      width = 12,
      box(
        title = "Welcome to SEM Batch Processor v2.0",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        
        h3("Enhanced Features"),
        tags$ul(
          tags$li(icon("check-circle"), " Dual Engine Support: Seamless switching between Mplus and R/lavaan"),
          tags$li(icon("check-circle"), " Intelligent Template System：Supports complexVariablesubstitution and conditional logic"),
          tags$li(icon("check-circle"), " Efficient Batch Processing: Parallel computation and progress tracking"),
          tags$li(icon("check-circle"), " AdvancedVisualization：publication-qualityPath Diagramand interactive charts"),
          tags$li(icon("check-circle"), " Comprehensive Error Handling: Automatic recovery and checkpoint mechanism")
        ),
        
        hr(),
        
        h4("QuickHome"),
        p("Follow these stepsHomeyourSEMAnalysis："),
        
        div(
          class = "wizard-step",
          span(class = "step-number", "1"),
          strong("Data Management："),
          "UploadyourDataset and proceedrowsQuality check"
        ),
        
        div(
          class = "wizard-step",
          span(class = "step-number", "2"),
          strong("Model Design："),
          "Select or createSEMModel template"
        ),
        
        div(
          class = "wizard-step",
          span(class = "step-number", "3"),
          strong("Batch Processing："),
          "Configure andRun Batch Analysis"
        ),
        
        div(
          class = "wizard-step",
          span(class = "step-number", "4"),
          strong("Results Analysis："),
          "View fit indices andParameter Estimates"
        ),
        
        div(
          class = "wizard-step",
          span(class = "step-number", "5"),
          strong("Visualization："),
          "GeneratePath DiagramAndresultsReport"
        ),
        
        hr(),
        
        actionButton(
          "start_workflow",
          "HomeWorkflow",
          icon = icon("play"),
          class = "btn-primary btn-lg",
          style = "width: 200px;"
        )
      )
    ),
    
    # System status
    column(
      width = 6,
      box(
        title = "System Status",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        
        valueBoxOutput("engine_status", width = 12),
        valueBoxOutput("data_status", width = 12),
        valueBoxOutput("model_status", width = 12)
      )
    ),
    

    
    # Recent activity
    column(
      width = 6,
      box(
        title = "Recent activity",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        
        uiOutput("recent_activity")
      )
    )
  )
}

#' Create data management tab
create_data_tab <- function() {
  fluidRow(
    column(
      width = 12,
      box(
        title = "Data import",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        
        fluidRow(
          column(
            width = 6,
            fileInput(
              "data_file",
              "Selectdata file",
              accept = c(".csv", ".sav", ".xlsx", ".dta"),
              buttonLabel = "Browse...",
              placeholder = "Support CSV, SPSS, Excel, Stata Format"
            ),
            
            selectInput(
              "data_encoding",
              "File encoding",
              choices = c("UTF-8", "GBK", "GB2312", "latin1"),
              selected = "UTF-8"
            )
          ),
          
          column(
            width = 6,
            h4("Or drag files here"),
            div(
              class = "drag-drop-zone",
              id = "drag_drop_zone",
              icon("cloud-upload", class = "fa-3x"),
              br(), br(),
              "Dragdata fileUpload here"
            )
          )
        ),
        
        hr(),
        
        actionButton("clear_data", "Clear Data", icon = icon("trash"), class = "btn-warning")
      )
    ),
    
    column(
      width = 12,
      box(
        title = "Data Preview",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        
        DTOutput("data_preview")
      )
    ),
    
    column(
      width = 6,
      box(
        title = "Descriptive Statistics",
        status = "success",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        
        DTOutput("descriptive_stats")
      )
    ),
    
    column(
      width = 6,
      box(
        title = "Missing data analysis",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        
        plotOutput("missing_data_plot", height = "300px"),
        DTOutput("missing_data_table")
      )
    )
  )
}

#' Create model design tab
create_model_tab <- function() {
  fluidRow(
    # Step 1: Design Mode Selection
    column(
      width = 12,
      box(
        title = "Step 1: SelectDesignMode",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        
        radioButtons(
          "model_design_mode",
          NULL,
          choices = c(
            "Manual DesignMode - Add models one by one" = "manual",
            "Auto-CombinationMode - BatchGenerateModel" = "auto"
          ),
          selected = "manual",
          inline = FALSE
        )
      )
    ),
    
    # Manual mode workflow
    conditionalPanel(
      condition = "input.model_design_mode == 'manual'",
      
      # Step 2: Template Selection
      column(
        width = 12,
        box(
          title = "Step 2: SelectOrEditTemplate",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          
          fluidRow(
            column(
              width = 3,
              selectInput(
                "template_select",
                "Select Template",
                choices = c(
                  "Custom" = "custom",
                  "BasicSEMModel" = "basic_sem",
                  "APIMModel" = "apim",
                  "Mediation model" = "mediation",
                  "Moderation model" = "moderation"
                )
              )
            ),
            column(
              width = 3,
              br(),
              actionButton("load_template", "Load Template", icon = icon("download"), class = "btn-info"),
              actionButton("clear_template", "Clear Template", icon = icon("eraser"), class = "btn-warning")
            )
          ),
          
          hr(),
          
          textAreaInput(
            "model_editor",
            "ModelEditProcessor",
            value = "",
            rows = 12,
            placeholder = "# Enter hereyourSEMModel\n# Use {{Placeholder}} Mark requiredmappingofVariable\n# Example:\n# {{outcome}} ~ {{predictor}}\n# {{mediator}} ~ {{predictor}}\n# {{outcome}} ~ {{mediator}}",
            width = "100%"
          ),
          
          # Only show validate button for R engine
          conditionalPanel(
            condition = "input.header_engine_select != 'mplus'",
            actionButton("validate_model", "ValidationModel", icon = icon("check"), class = "btn-info")
          ),
          
          verbatimTextOutput("model_validation_output")
        )
      ),
      
      # Step 3: Variable Mapping (only show if template has placeholders)
      conditionalPanel(
        condition = "output.has_placeholders",
        column(
          width = 12,
          box(
            title = "Step 3: mappingVariable",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            
            uiOutput("variable_mapping_ui"),
            
            hr(),
            
            fluidRow(
              column(
                width = 6,
                textInput(
                  "model_name_mapping",
                  "Model Name",
                  placeholder = "For example：Model1 - SatisfactionpredictionStress"
                )
              ),
              column(
                width = 6,
                br(),
                actionButton("apply_and_save", "ApplymappingAnd save", icon = icon("save"), class = "btn-success btn-block")
              )
            ),
            
            
            br(),
            
            uiOutput("mapping_validation_message")
          )
        )
      ),
      
    ),
    
    # Auto combination mode workflow
    conditionalPanel(
      condition = "input.model_design_mode == 'auto'",
      
      # Step 2: Template Selection
      column(
        width = 12,
        box(
          title = "Step 2: Select Template",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          
          fluidRow(
            column(
              width = 6,
              selectInput(
                "combo_template_select",
                "Select Template",
                choices = c(
                  "Custom" = "custom",
                  "BasicSEMModel" = "basic_sem",
                  "APIMModel" = "apim",
                  "Mediation model" = "mediation",
                  "Moderation model" = "moderation"
                )
              )
            ),
            column(
              width = 6,
              br(),
              div(
                style = "display: flex; gap: 10px;",
                actionButton("load_combo_template", "Load Template", icon = icon("download"), class = "btn-info"),
                actionButton("clear_combo_template", "Clear Template", icon = icon("trash"), class = "btn-warning")
              )
            )
          ),
          
          hr(),
          
          conditionalPanel(
            condition = "input.combo_template_select == 'custom'",
            h5("CustomTemplateEditProcessor"),
            textAreaInput(
              "combo_custom_editor",
              NULL,
              value = "",
              rows = 10,
              placeholder = "# InputCustomTemplate\n# Use {{Placeholder}} Mark requiredmappingofVariable\n# Example:\n# {{outcome}} ~ {{predictor}}"
            )
          ),
          
          conditionalPanel(
            condition = "input.combo_template_select != 'custom'",
            h5("Template preview"),
            verbatimTextOutput("combo_template_preview")
          )
        )
      ),
      
      # Step 3: Multi-variable Selection
      column(
        width = 12,
        box(
          title = "Step 3: Select multipleVariable",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          
          uiOutput("multi_mapping_ui"),
          
          hr(),
          
          uiOutput("combination_info")
        )
      ),
      
      # Step 4: Generate Combinations
      column(
        width = 12,
        box(
          title = "Step 4: GenerateModelcombinations",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          
          actionButton(
            "generate_combinations",
            "Generate All Combinations",
            icon = icon("magic"),
            class = "btn-success btn-lg"
          )
        )
      ),
      
    ),
    
    # Unified Model Library (visible in both modes)
    column(
      width = 12,
      box(
        title = "CurrentModel Library",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        
        uiOutput("model_library_ui"),
        
        hr(),
        
        actionButton("clear_all_models", "ClearModel Library", icon = icon("trash"), class = "btn-danger")
      )
    )
  )
}

#' Create batch processing tab
create_batch_tab <- function() {
  fluidRow(
    # Mplus specific controls (only show when Mplus engine is selected)
    conditionalPanel(
      condition = "input.header_engine_select == 'mplus'",
      column(
        width = 12,
        box(
          title = "MplusBatch Processing - Step1: GenerateinpFile",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          
          fluidRow(
            column(
              width = 6,
              textInput(
                "mplus_output_dir",
                "Output folder path",
                value = file.path(getwd(), "mplus_output"),
                placeholder = "SelectGenerateinpAnddatFile location"
              )
            ),
            column(
              width = 2,
              br(),
              actionButton(
                "browse_output_dir",
                "Browse...",
                icon = icon("folder-open"),
                class = "btn-default"
              )
            ),
            column(
              width = 4,
              br(),
              actionButton(
                "generate_mplus_files",
                "GenerateAllinpFile",
                icon = icon("file-code"),
                class = "btn-warning btn-lg btn-block"
              )
            )
          ),
          
          hr(),
          
          verbatimTextOutput("mplus_generation_log")
        )
      ),
      
      column(
        width = 12,
        box(
          title = "MplusBatch Processing - Step2: RunningModel",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          
          fluidRow(
            column(
              width = 6,
              textInput(
                "mplus_run_dir",
                "inpFolder path",
                value = file.path(getwd(), "mplus_output"),
                placeholder = "SelectpackageContainsinpFile folder"
              )
            ),
            column(
              width = 2,
              br(),
              actionButton(
                "browse_run_dir",
                "Browse...",
                icon = icon("folder-open"),
                class = "btn-default"
              )
            ),
            column(
              width = 4,
              br(),
              actionButton(
                "run_mplus_batch",
                "Batch runMplus",
                icon = icon("play"),
                class = "btn-success btn-lg btn-block"
              )
            )
          )
        )
      )
    ),
    
    # R/lavaan engine controls (original)
    conditionalPanel(
      condition = "input.header_engine_select != 'mplus'",
      column(
        width = 12,
        box(
          title = "Batch ProcessingConfiguration",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          
          fluidRow(
            column(
              width = 4,
              numericInput(
                "max_parallel_jobs",
                "AndrowsNumber of tasks",
                value = 4,
                min = 1,
                max = 16
              )
            ),
            
            column(
              width = 4,
              numericInput(
                "checkpoint_interval",
                "Checkpoint interval",
                value = 10,
                min = 1,
                max = 100
              )
            ),
            
            column(
              width = 4,
              checkboxInput(
                "auto_retry",
                "Auto retry on failure",
                value = TRUE
              )
            )
          ),
          
          hr(),
          
          actionButton("start_batch", "HomeBatch Processing", icon = icon("play"), class = "btn-success btn-lg"),
          actionButton("pause_batch", "Pause", icon = icon("pause"), class = "btn-warning"),
          actionButton("stop_batch", "Stop", icon = icon("stop"), class = "btn-danger")
        )
      )
    ),
    
    column(
      width = 12,
      box(
        title = "Processing Progress",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        
        uiOutput("batch_progress_bar"),
        
        br(),
        
        fluidRow(
          valueBoxOutput("models_completed", width = 4),
          valueBoxOutput("models_failed", width = 4),
          valueBoxOutput("estimated_time", width = 4)
        ),
        
        hr(),
        
        verbatimTextOutput("batch_log", placeholder = TRUE)
      )
    ),
    
    column(
      width = 6,
      box(
        title = "Pending ModelscolumnsTable",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        
        uiOutput("pending_models_list")
      )
    )
  )
}

#' Create results analysis tab
create_results_tab <- function() {
  fluidRow(
    column(
      width = 12,
      box(
        title = "Model selection",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        
        uiOutput("result_model_selector")
      )
    ),
    
    column(
      width = 12,
      box(
        title = "resultsOverview",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        
        DTOutput("results_summary")
      )
    ),
    
    column(
      width = 12,
      box(
        title = "Parameter Estimates",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        
        DTOutput("parameter_estimates")
      )
    )
  )
}

#' Create visualization tab
create_visualization_tab <- function() {
  fluidRow(
    column(
      width = 3,
      box(
        title = "VisualizationOptions",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        
        selectInput(
          "plot_type",
          "Chart type",
          choices = c(
            "Fit Indices Comparison" = "fit_compare",
            "Fit Indices Distribution" = "param_dist",
            "Chi-Square Comparison" = "chisq_compare",
            "Path Diagram" = "path"
          )
        ),
        
        # Model selector (only for path diagram)
        conditionalPanel(
          condition = "input.plot_type == 'path'",
          hr(),
          uiOutput("viz_model_selector")
        ),
        
        selectInput(
          "plot_format",
          "Export format",
          choices = c("PNG", "PDF", "SVG")
        ),
        
        numericInput(
          "plot_width",
          "Width (Inches)",
          value = 10,
          min = 4,
          max = 20
        ),
        
        numericInput(
          "plot_height",
          "Height (Inches)",
          value = 8,
          min = 4,
          max = 20
        ),
        
        actionButton("generate_plot", "GenerateChart", icon = icon("chart-line"), class = "btn-primary btn-block"),
        downloadButton("download_plot", "Download Chart", class = "btn-success btn-block")
      )
    ),
    
    column(
      width = 9,
      box(
        title = "Chart preview",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        
        plotOutput("visualization_plot", height = "600px")
      )
    )
  )
}

#' Create settings tab
create_settings_tab <- function() {
  fluidRow(
    # R/lavaan engine status (Display first)
    column(
      width = 6,
      box(
        title = "R/lavaan Engine status",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        
        h4("Engine information"),
        verbatimTextOutput("r_engine_status"),
        
        hr(),
        
        helpText("R/lavaan Is default recommended engine，Full-featured and free。Suitable for mostSEMAnalysis needs。")
      )
    ),
    
    # Mplus engine status (Display later)
    column(
      width = 6,
      box(
        title = "Mplus Engine status",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        
        h4("Engine information"),
        verbatimTextOutput("mplus_engine_status"),
        
        hr(),
        
        helpText("Mplus Requires separate installation and license。Suitable for complex models and classificationVariableAnalysis。")
      )
    ),
    
    # Engine selection
    column(
      width = 12,
      box(
        title = "Engine selection",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        
        radioButtons(
          "default_engine",
          "CurrentUseAnalysis engine",
          choices = c("R/lavaan (Recommended)" = "r", "Mplus (Alternative)" = "mplus"),
          selected = "r",
          inline = TRUE
        )
      )
    ),
    
    column(
      width = 6,
      box(
        title = "InterfaceSettings",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        
        selectInput(
          "ui_theme",
          "InterfaceTheme",
          choices = c("Default" = "flatly", "Dark" = "darkly", "Simple" = "cosmo")
        ),
        
        selectInput(
          "ui_language",
          "InterfaceLanguage",
          choices = c("Simplified Chinese" = "zh-CN", "English" = "en-US")
        ),
        
        checkboxInput("enable_tooltips", "Display toolsTip", value = TRUE),
        checkboxInput("enable_animations", "Enable animation effects", value = TRUE)
      )
    ),
    
    column(
      width = 12,
      box(
        title = "AdvancedSettings",
        status = "warning",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        
        numericInput("log_level", "Log level (1-5)", value = 2, min = 1, max = 5),
        numericInput("max_memory_mb", "Maximum memory (MB)", value = 4096, min = 512, max = 16384),
        checkboxInput("enable_cache", "Enable cache", value = TRUE),
        
        hr(),
        
        actionButton("save_settings", "SaveSettings", icon = icon("save"), class = "btn-success"),
        actionButton("reset_settings", "Reset to default", icon = icon("undo"), class = "btn-warning")
      )
    )
  )
}

#' Create help tab
create_help_tab <- function() {
  fluidRow(
    column(
      width = 12,
      box(
        title = "UseHelp",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        
        h3("QuickGetting started guide"),
        p("WelcomeUse SEM Batch ProcessingProcessor！This guide willHelpYouQuickGet started。"),
        
        h4("1. DataPreparing"),
        p("Supported data formats：CSV, SPSS (.sav), Excel (.xlsx), Stata (.dta)"),
        p("EnsureyourData："),
        tags$ul(
          tags$li("FirstrowsAsVariableName"),
          tags$li("EachrowsRepresents one observation"),
          tags$li("MissingUse NA Or -999 Indicates")
        ),
        
        h4("2. Model Design"),
        p("You can："),
        tags$ul(
          tags$li("Select preset models from template library"),
          tags$li("Use Mplus Or lavaan Syntax writingCustomModel"),
          tags$li("Use {{VariableName}} AsPlaceholderEnterrowsBatch replace")
        ),
        
        h4("3. Batch Processing"),
        p("System will automatically："),
        tags$ul(
          tags$li("GenerateAllVariablecombinationsmodels"),
          tags$li("AndrowsExecuterowsModel estimation"),
          tags$li("Save checkpoints for recovery"),
          tags$li("Handle errors and retry failed models")
        ),
        
        h4("4. Results Analysis"),
        p("View："),
        tags$ul(
          tags$li("Model fit indices (CFI, TLI, RMSEA, SRMR)"),
          tags$li("Parameter EstimatesAndSignificance test"),
          tags$li("Standardized coefficients"),
          tags$li("Confidence interval")
        ),
        
        hr(),
        
        h3("FAQ"),
        
        tags$details(
          tags$summary(strong("Q: How to choose appropriate analysis engine？")),
          p("A: System will automatically detect available engines。Mplus Suitable for complex models and classificationVariable，R/lavaan Open source free andVisualizationGood performance。")
        ),
        
        tags$details(
          tags$summary(strong("Q: Batch ProcessingWhat to do if failed？")),
          p("A: System will automatically save checkpoints，You can continue from interruption。View log to understand failure reason。")
        ),
        
        tags$details(
          tags$summary(strong("Q: How toExport Results？")),
          p("A: InResults AnalysisPage，Can export as Excel, CSV Or PDF Format。")
        ),
        
        hr(),
        
        h3("Contact support"),
        p("If you have questions or suggestions，Please contact via following methods："),
        tags$ul(
          tags$li(icon("envelope"), " Email: zhengqg@mail.bnu.edu.cn"),
          tags$li(icon("github"), " GitHub: https://github.com/VictoirZheng"),
          tags$li(icon("user"), " Author: Qingguang \"Victor\" Zheng")
        )
      )
    )
  )
}

# Export UI
ui <- create_main_ui()

