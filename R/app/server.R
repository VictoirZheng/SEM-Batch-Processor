# Main server: shared state, home page, sidebar, settings.

app_server <- function(input, output, session) {
  cfg <- APP_CONFIG
  q <- shiny::parseQueryString(isolate(session$clientData$url_search) %||% "")
  lang <- normalize_lang(q$lang %||% cfg$language)
  tr <- make_tr(lang)

  # Per-session working directory (deleted when the session ends)
  sess_dir <- file.path(tempdir(), paste0("sembp_", session$token))
  dir.create(sess_dir, recursive = TRUE, showWarnings = FALSE)

  rv <- reactiveValues(
    data_raw = NULL, data = NULL, data_info = NULL,
    missing_codes = numeric(0), missing_counts = integer(0), derive_ops = list(),
    library = list(), next_model = 1L,
    batch = NULL, results = list(), progress = NULL, running = FALSE, log = character(0),
    mplus_path = cfg$mplus_path
  )
  # Non-reactive holder for the job queue (an environment)
  state <- new.env(parent = emptyenv())

  ctx <- list(input = input, output = output, session = session, rv = rv, state = state,
              tr = tr, lang = lang, cfg = cfg, sess_dir = sess_dir)

  server_data(ctx)
  server_model(ctx)
  server_run(ctx)
  server_results(ctx)
  server_plots(ctx)

  # --- Home ---------------------------------------------------------------
  output$vb_data <- shinydashboard::renderValueBox({
    d <- rv$data
    shinydashboard::valueBox(
      if (is.null(d)) "—" else sprintf("%d × %d", nrow(d), ncol(d)),
      tr("vb_data"), icon = icon("database"), color = if (is.null(d)) "yellow" else "green")
  })
  output$vb_models <- shinydashboard::renderValueBox({
    n <- length(rv$library)
    shinydashboard::valueBox(n, tr("vb_models"), icon = icon("layer-group"),
                             color = if (n == 0) "yellow" else "green")
  })
  output$vb_results <- shinydashboard::renderValueBox({
    p <- rv$progress
    val <- if (is.null(p)) "—" else sprintf("%d / %d", p$done, p$total)
    shinydashboard::valueBox(val, tr("vb_results"), icon = icon("table"),
                             color = if (is.null(p)) "yellow" else if (isTRUE(rv$running)) "aqua" else "green")
  })
  output$vb_engine <- shinydashboard::renderValueBox({
    eng <- input$engine %||% "lavaan"
    shinydashboard::valueBox(if (eng == "mplus") "Mplus" else "lavaan", tr("vb_engine"),
                             icon = icon("gears"), color = "blue")
  })
  output$home_engines <- renderUI(engine_status_ui(ctx))
  observeEvent(input$go_data, shinydashboard::updateTabItems(session, "tabs", "data"))

  output$sidebar_status <- renderUI({
    items <- list(
      c(tr("sb_data"), if (is.null(rv$data)) tr("sb_none") else sprintf("%d × %d", nrow(rv$data), ncol(rv$data))),
      c(tr("sb_models"), length(rv$library)),
      c(tr("sb_run"), if (isTRUE(rv$running)) sprintf("%d / %d", rv$progress$done, rv$progress$total)
        else if (!is.null(rv$progress)) tr("sb_done") else tr("sb_none"))
    )
    div(class = "sidebar-status",
        lapply(items, function(x) div(span(class = "k", x[1]), span(class = "v", x[2]))))
  })

  # --- Settings -----------------------------------------------------------
  output$settings_engines <- renderUI(engine_status_ui(ctx))
  observeEvent(input$apply_lang, {
    session$sendCustomMessage("sembp-set-lang", input$set_lang)
  })
  observeEvent(input$set_mplus_check, {
    p <- detect_mplus(input$set_mplus_path)
    rv$mplus_path <- p
    if (nzchar(p)) showNotification(tr("set_mplus_found", p), type = "message")
    else showNotification(tr("set_mplus_notfound"), type = "warning")
  })

  session$onSessionEnded(function() {
    if (!is.null(state$queue)) try(queue_cancel(state$queue), silent = TRUE)
    unlink(sess_dir, recursive = TRUE)
  })
}

#' Small table describing the available engines
engine_status_ui <- function(ctx) {
  tr <- ctx$tr
  cfg <- ctx$cfg
  mp <- ctx$rv$mplus_path %||% ""
  lav <- tryCatch(as.character(utils::packageVersion("lavaan")), error = function(e) NA)
  rows <- list(
    c("R / lavaan", if (is.na(lav)) tr("eng_missing") else paste(tr("eng_available"), lav)),
    c("Mplus", if (cfg$mode == "online") tr("eng_online_disabled")
      else if (nzchar(mp)) paste(tr("eng_available"), mp) else tr("eng_not_found")),
    c("MplusAutomation", if (isTRUE(cfg$mplus_automation)) tr("eng_installed") else tr("eng_not_installed"))
  )
  tags$table(class = "table table-condensed engine-table",
             tags$tbody(lapply(rows, function(r) tags$tr(tags$th(r[1]), tags$td(r[2])))))
}
