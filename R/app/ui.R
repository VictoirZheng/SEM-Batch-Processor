# User interface (shinydashboard). The UI is built per request so that the
# language can be chosen with ?lang=zh / ?lang=en.

app_ui <- function(request) {
  q <- shiny::parseQueryString(request$QUERY_STRING %||% "")
  lang <- normalize_lang(q$lang %||% APP_CONFIG$language)
  tr <- make_tr(lang)
  cfg <- APP_CONFIG

  shinydashboard::dashboardPage(
    skin = "blue",
    title = "SEM Batch Processor",
    shinydashboard::dashboardHeader(
      title = "SEM Batch Processor",
      titleWidth = 260,
      tags$li(class = "dropdown mode-badge",
              tags$span(class = paste("label", if (cfg$mode == "online") "label-info" else "label-success"),
                        if (cfg$mode == "online") tr("mode_online") else tr("mode_local")))
    ),
    shinydashboard::dashboardSidebar(
      width = 260,
      shinydashboard::sidebarMenu(
        id = "tabs",
        shinydashboard::menuItem(tr("nav_home"), tabName = "home", icon = icon("house")),
        shinydashboard::menuItem(tr("nav_data"), tabName = "data", icon = icon("database")),
        shinydashboard::menuItem(tr("nav_model"), tabName = "model", icon = icon("diagram-project")),
        shinydashboard::menuItem(tr("nav_run"), tabName = "run", icon = icon("play")),
        shinydashboard::menuItem(tr("nav_results"), tabName = "results", icon = icon("table")),
        shinydashboard::menuItem(tr("nav_plots"), tabName = "plots", icon = icon("chart-line")),
        shinydashboard::menuItem(tr("nav_settings"), tabName = "settings", icon = icon("gear")),
        shinydashboard::menuItem(tr("nav_help"), tabName = "help", icon = icon("circle-question"))
      ),
      uiOutput("sidebar_status")
    ),
    shinydashboard::dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
        tags$script(src = "app.js")
      ),
      shinydashboard::tabItems(
        shinydashboard::tabItem("home", ui_home(tr, cfg)),
        shinydashboard::tabItem("data", ui_data(tr, cfg)),
        shinydashboard::tabItem("model", ui_model(tr, cfg)),
        shinydashboard::tabItem("run", ui_run(tr, cfg)),
        shinydashboard::tabItem("results", ui_results(tr)),
        shinydashboard::tabItem("plots", ui_plots(tr)),
        shinydashboard::tabItem("settings", ui_settings(tr, cfg, lang)),
        shinydashboard::tabItem("help", help_content(lang))
      )
    )
  )
}

# Named ui_box (not "box") so that graphics::box() is not masked in the
# global environment.
ui_box <- function(..., title = NULL, status = "primary", width = 12, collapsible = FALSE,
                collapsed = FALSE) {
  shinydashboard::box(..., title = title, status = status, solidHeader = FALSE,
                      width = width, collapsible = collapsible, collapsed = collapsed)
}

ui_home <- function(tr, cfg) {
  fluidRow(
    shinydashboard::valueBoxOutput("vb_data", width = 3),
    shinydashboard::valueBoxOutput("vb_models", width = 3),
    shinydashboard::valueBoxOutput("vb_results", width = 3),
    shinydashboard::valueBoxOutput("vb_engine", width = 3),
    ui_box(title = tr("home_welcome_title"), width = 7,
        p(tr("home_intro")),
        tags$ol(class = "steps",
                tags$li(HTML(tr("home_step1"))),
                tags$li(HTML(tr("home_step2"))),
                tags$li(HTML(tr("home_step3"))),
                tags$li(HTML(tr("home_step4")))),
        actionButton("go_data", tr("home_start"), icon = icon("arrow-right"), class = "btn-primary")
    ),
    ui_box(title = tr("home_mode_title"), width = 5, status = if (cfg$mode == "online") "info" else "success",
        p(HTML(if (cfg$mode == "online") tr("home_mode_online") else tr("home_mode_local"))),
        uiOutput("home_engines"))
  )
}

ui_data <- function(tr, cfg) {
  fluidRow(
    column(4,
      ui_box(title = tr("data_upload_title"), width = 12,
          fileInput("data_file", tr("data_file"), accept = paste0(".", SUPPORTED_DATA_EXT),
                    buttonLabel = tr("browse"), placeholder = tr("data_no_file")),
          fluidRow(
            column(7, selectInput("data_encoding", tr("data_encoding"),
                                  c(stats::setNames("auto", tr("data_encoding_auto")),
                                    "UTF-8" = "UTF-8", "GBK / GB18030" = "GB18030"))),
            column(5, numericInput("data_sheet", tr("data_sheet"), 1, min = 1, step = 1))
          ),
          helpText(tr("data_formats_help", cfg$limit$max_upload_mb))),
      ui_box(title = tr("data_missing_title"), width = 12,
          textInput("missing_codes", tr("data_missing_codes"), placeholder = "-999, -99"),
          actionButton("apply_missing", tr("apply"), icon = icon("check")),
          helpText(tr("data_missing_help")),
          uiOutput("missing_report")),
      ui_box(title = tr("derive_title"), width = 12, collapsible = TRUE, collapsed = TRUE,
          radioButtons("derive_type", NULL,
                       stats::setNames(c("center", "product", "mean"),
                                       c(tr("derive_center"), tr("derive_product"), tr("derive_mean")))),
          selectizeInput("derive_vars", tr("derive_vars"), choices = NULL, multiple = TRUE),
          conditionalPanel("input.derive_type == 'product'",
                           checkboxInput("derive_center_product", tr("derive_center_first"), TRUE)),
          conditionalPanel("input.derive_type == 'mean'",
                           textInput("derive_name", tr("derive_name"), "score"),
                           numericInput("derive_min_items", tr("derive_min_items"), 1, min = 1)),
          actionButton("derive_add", tr("derive_add"), icon = icon("plus")),
          helpText(tr("derive_help")),
          uiOutput("derive_log"))
    ),
    column(8,
      uiOutput("data_summary"),
      shinydashboard::tabBox(
        width = 12, id = "data_tabs",
        tabPanel(tr("data_preview"), DT::DTOutput("data_preview")),
        tabPanel(tr("data_describe"), helpText(tr("data_describe_help")), DT::DTOutput("data_describe")),
        tabPanel(tr("data_missing_tab"), plotOutput("missing_plot", height = "320px"), DT::DTOutput("missing_table")),
        tabPanel(tr("data_names_tab"), helpText(tr("data_names_help")), DT::DTOutput("names_table"))
      )
    )
  )
}

ui_model <- function(tr, cfg) {
  engine_choices <- stats::setNames(c("lavaan", "mplus"), c("R / lavaan", "Mplus"))
  fluidRow(
    column(7,
      ui_box(title = tr("model_template_title"), width = 12,
          fluidRow(
            column(4, radioButtons("engine", tr("engine"), engine_choices, selected = "lavaan", inline = TRUE),
                   uiOutput("engine_note")),
            column(5, selectInput("template_select", tr("model_template"), choices = NULL)),
            column(3, br(), actionButton("load_template", tr("load"), icon = icon("file-import"), class = "btn-block"))
          ),
          textAreaInput("template_text", NULL, value = "", rows = 16, width = "100%",
                        placeholder = tr("model_editor_placeholder")),
          helpText(HTML(tr("model_placeholder_help"))),
          uiOutput("template_check"))
    ),
    column(5,
      ui_box(title = tr("model_assign_title"), width = 12, status = "warning",
          uiOutput("placeholder_ui"),
          checkboxInput("exclude_duplicates", tr("model_exclude_dup"), TRUE),
          textInput("model_prefix", tr("model_name_prefix"), ""),
          uiOutput("combo_info"),
          div(class = "btn-row",
              actionButton("preview_model", tr("model_preview"), icon = icon("eye")),
              actionButton("add_models", tr("model_add"), icon = icon("plus"), class = "btn-success")))
    ),
    column(12,
      ui_box(title = tr("library_title"), width = 12, status = "success",
          uiOutput("library_info"),
          DT::DTOutput("library_table"),
          div(class = "btn-row",
              actionButton("library_view", tr("library_view"), icon = icon("eye")),
              actionButton("library_remove", tr("library_remove"), icon = icon("trash")),
              actionButton("library_clear", tr("library_clear"), icon = icon("broom"), class = "btn-danger"))))
  )
}

ui_run <- function(tr, cfg) {
  fluidRow(
    column(5,
      conditionalPanel("input.engine == 'lavaan'",
        ui_box(title = tr("run_lavaan_title"), width = 12,
            fluidRow(
              column(6, selectInput("opt_estimator", tr("opt_estimator"),
                                    stats::setNames(LAVAAN_ESTIMATORS,
                                                    c("ML", tr("est_mlr"), "MLM (Satorra-Bentler)", "MLMV",
                                                      tr("est_wlsmv"), "ULSMV")))),
              column(6, selectInput("opt_missing", tr("opt_missing"),
                                    stats::setNames(c("fiml", "fiml.x", "listwise"),
                                                    c(tr("miss_fiml"), tr("miss_fimlx"), tr("miss_listwise")))))
            ),
            fluidRow(
              column(6, numericInput("opt_bootstrap", tr("opt_bootstrap"), 0, min = 0,
                                     max = cfg$limit$max_bootstrap, step = 500)),
              column(6, selectInput("opt_boot_ci", tr("opt_boot_ci"),
                                    stats::setNames(BOOT_CI_TYPES,
                                                    c(tr("ci_perc"), tr("ci_bc"), "BCa", tr("ci_norm")))))
            ),
            fluidRow(
              column(6, numericInput("opt_seed", tr("opt_seed"), 12345, min = 1, step = 1)),
              column(6, numericInput("opt_level", tr("opt_level"), 0.95, min = 0.5, max = 0.999, step = 0.01))
            ),
            checkboxInput("opt_std_lv", tr("opt_std_lv"), FALSE),
            selectizeInput("opt_ordered", tr("opt_ordered"), choices = NULL, multiple = TRUE),
            uiOutput("option_notes"),
            helpText(HTML(tr("run_lavaan_help"))))),
      conditionalPanel("input.engine == 'mplus'",
        ui_box(title = tr("run_mplus_title"), width = 12,
            p(HTML(tr("run_mplus_help"))),
            if (cfg$mode == "local") textInput("mplus_out_dir", tr("run_mplus_dir"),
                                               file.path(getwd(), "mplus_runs")),
            uiOutput("mplus_status")))
    ),
    column(7,
      ui_box(title = tr("run_title"), width = 12, status = "success",
          fluidRow(
            column(4, numericInput("run_workers", tr("run_workers"),
                                   min(2, cfg$limit$max_workers), min = 1, max = cfg$limit$max_workers)),
            column(4, numericInput("run_timeout", tr("run_timeout"), cfg$limit$default_timeout_s,
                                   min = 10, step = 60)),
            column(4, br(), checkboxInput("run_keep_fits", tr("run_keep_fits"), TRUE))
          ),
          uiOutput("run_summary"),
          div(class = "btn-row",
              actionButton("run_start", tr("run_start"), icon = icon("play"), class = "btn-success btn-lg"),
              actionButton("run_cancel", tr("run_cancel"), icon = icon("stop"), class = "btn-danger btn-lg")),
          br(),
          uiOutput("run_progress"),
          verbatimTextOutput("run_log", placeholder = TRUE)))
  )
}

ui_results <- function(tr) {
  fluidRow(
    column(12, uiOutput("results_banner")),
    shinydashboard::tabBox(
      width = 12, id = "results_tabs",
      tabPanel(tr("res_fit_tab"),
               fluidRow(column(8, helpText(HTML(tr("res_fit_help")))),
                        column(4, checkboxInput("fit_only_ok", tr("res_only_ok"), FALSE))),
               DT::DTOutput("fit_table")),
      tabPanel(tr("res_focal_tab"),
               fluidRow(
                 column(3, checkboxGroupInput("focal_types", tr("res_focal_types"),
                                              stats::setNames(FOCAL_TYPES,
                                                              c(tr("ft_defined"), tr("ft_indirect"), tr("ft_total"),
                                                                tr("ft_regression"), tr("ft_loading"), tr("ft_covariance"))),
                                              selected = c("defined", "indirect"))),
                 column(3, textInput("focal_labels", tr("res_focal_labels"), placeholder = "ind, a, b"),
                        checkboxInput("focal_inadmissible", tr("res_include_inadmissible"), FALSE)),
                 column(3, selectInput("p_adjust", tr("res_p_adjust"),
                                       stats::setNames(P_ADJUST_METHODS,
                                                       c("Holm", "Benjamini-Hochberg (FDR)", "Bonferroni", tr("padj_none")))),
                        radioButtons("p_family", tr("res_family"),
                                     stats::setNames(c("all", "effect"), c(tr("family_all"), tr("family_effect"))))),
                 column(3, numericInput("alpha", "α", 0.05, min = 0.001, max = 0.2, step = 0.01),
                        uiOutput("focal_family_info"))
               ),
               helpText(HTML(tr("res_focal_help"))),
               h4(tr("res_effect_summary")),
               DT::DTOutput("focal_summary"),
               h4(tr("res_focal_all")),
               DT::DTOutput("focal_table")),
      tabPanel(tr("res_detail_tab"),
               fluidRow(column(6, selectInput("detail_model", tr("res_select_model"), choices = NULL)),
                        column(6, checkboxInput("detail_all_params", tr("res_all_params"), FALSE))),
               uiOutput("detail_status"),
               DT::DTOutput("detail_params"),
               fluidRow(column(6, h4(tr("res_syntax")), verbatimTextOutput("detail_syntax")),
                        column(6, h4(tr("res_output")), div(class = "raw-output", verbatimTextOutput("detail_output"))))),
      tabPanel(tr("res_export_tab"),
               p(tr("res_export_help")),
               div(class = "btn-row",
                   downloadButton("dl_xlsx", tr("res_dl_xlsx"), class = "btn-success"),
                   downloadButton("dl_zip", tr("res_dl_zip")),
                   downloadButton("dl_focal_csv", tr("res_dl_csv"))),
               br(),
               h4(tr("res_methods")),
               tableOutput("methods_table"))
    )
  )
}

ui_plots <- function(tr) {
  fluidRow(
    column(3,
      ui_box(title = tr("plot_options"), width = 12,
          selectInput("plot_type", tr("plot_type"),
                      stats::setNames(c("spec", "fitdist", "path"),
                                      c(tr("plot_spec"), tr("plot_fitdist"), tr("plot_path")))),
          conditionalPanel("input.plot_type == 'spec'",
                           selectInput("plot_effect", tr("plot_effect"), choices = NULL),
                           checkboxInput("plot_std", tr("plot_std"), FALSE)),
          conditionalPanel("input.plot_type == 'path'",
                           selectInput("plot_model", tr("res_select_model"), choices = NULL),
                           helpText(tr("plot_path_help"))),
          hr(),
          fluidRow(column(6, numericInput("plot_w", tr("plot_width"), 9, min = 3, max = 30)),
                   column(6, numericInput("plot_h", tr("plot_height"), 6, min = 3, max = 30))),
          selectInput("plot_format", tr("plot_format"), c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg")),
          downloadButton("dl_plot", tr("plot_download"), class = "btn-block"))),
    column(9, ui_box(title = tr("plot_preview"), width = 12, plotOutput("main_plot", height = "560px")))
  )
}

ui_settings <- function(tr, cfg, lang) {
  fluidRow(
    ui_box(title = tr("set_language"), width = 6,
        selectInput("set_lang", NULL, c("中文" = "zh", "English" = "en"), selected = lang),
        actionButton("apply_lang", tr("apply"), icon = icon("language")),
        helpText(tr("set_language_help"))),
    ui_box(title = tr("set_engines"), width = 6,
        uiOutput("settings_engines"),
        if (cfg$mode == "local") tagList(
          textInput("set_mplus_path", tr("set_mplus_path"), cfg$mplus_path),
          actionButton("set_mplus_check", tr("set_mplus_check"), icon = icon("magnifying-glass")),
          helpText(tr("set_mplus_help"))
        ) else helpText(tr("set_online_no_mplus")))
  )
}
