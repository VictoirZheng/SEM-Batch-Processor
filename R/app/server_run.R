# Run page: estimation options, starting/cancelling the background queue.

server_run <- function(ctx) {
  input <- ctx$input; output <- ctx$output; session <- ctx$session
  rv <- ctx$rv; state <- ctx$state; tr <- ctx$tr; cfg <- ctx$cfg

  engine <- reactive(input$engine %||% "lavaan")

  lavaan_opts <- reactive(list(
    estimator = input$opt_estimator %||% "ML",
    missing = input$opt_missing %||% "fiml",
    bootstrap = min(as.integer(input$opt_bootstrap %||% 0), cfg$limit$max_bootstrap),
    boot_ci = input$opt_boot_ci %||% "perc",
    seed = as.integer(input$opt_seed %||% 12345),
    std_lv = isTRUE(input$opt_std_lv),
    ordered = input$opt_ordered %||% character(0),
    level = as.numeric(input$opt_level %||% 0.95)
  ))

  output$option_notes <- renderUI({
    r <- resolve_lavaan_options(lavaan_opts())
    if (length(r$notes) == 0) return(NULL)
    div(class = "callout callout-info small", tr("opt_adjusted"), tags$ul(lapply(r$notes, tags$li)))
  })

  output$mplus_status <- renderUI({
    mp <- rv$mplus_path %||% ""
    items <- list(
      if (nzchar(mp)) div(class = "text-success", icon("check"), " ", tr("mplus_path_is", mp))
      else div(class = "text-danger", icon("xmark"), " ", tr("mplus_not_found")),
      if (isTRUE(cfg$mplus_automation)) div(class = "text-success", icon("check"), " MplusAutomation")
      else div(class = "text-danger", icon("xmark"), " ", tr("mplus_need_ma"))
    )
    tagList(items)
  })

  models_for_engine <- reactive({
    lib <- rv$library
    lib[vapply(lib, function(m) identical(m$engine, engine()), logical(1))]
  })

  output$run_summary <- renderUI({
    n <- length(models_for_engine())
    if (n == 0) return(div(class = "callout callout-warning", tr("run_no_models")))
    boot <- if (engine() == "lavaan") lavaan_opts()$bootstrap else 0
    div(class = "callout callout-info",
        HTML(tr("run_ready", n, if (engine() == "mplus") "Mplus" else "lavaan")),
        if (boot > 0) div(class = "small", tr("run_boot_note", boot)))
  })

  # --- Start ------------------------------------------------------------------
  observeEvent(input$run_start, {
    if (isTRUE(rv$running)) return(showNotification(tr("run_already"), type = "warning"))
    if (is.null(rv$data)) return(showNotification(tr("ph_need_data"), type = "warning"))
    models <- models_for_engine()
    if (length(models) == 0) return(showNotification(tr("run_no_models"), type = "warning"))
    eng <- engine()

    stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    base_dir <- if (eng == "mplus" && cfg$mode == "local" && nzchar(input$mplus_out_dir %||% "")) input$mplus_out_dir else ctx$sess_dir
    run_dir <- file.path(base_dir, paste0("run_", stamp))
    ok <- dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
    if (!ok && !dir.exists(run_dir)) return(showNotification(tr("run_dir_error", run_dir), type = "error"))

    manifest <- build_manifest(models)
    data <- rv$data
    notes <- character(0)
    workers <- max(1, min(as.integer(input$run_workers %||% 1), cfg$limit$max_workers))
    timeout <- as.numeric(input$run_timeout %||% cfg$limit$default_timeout_s)

    prep <- tryCatch({
      if (eng == "lavaan") {
        opts <- lavaan_opts()
        notes <- resolve_lavaan_options(opts)$notes
        data_rds <- file.path(run_dir, "data.rds")
        saveRDS(data, data_rds)
        tasks <- lapply(seq_len(nrow(manifest)), function(i) list(id = manifest$id[i], syntax = manifest$syntax[i]))
        q <- create_job_queue(tasks, "lavaan", run_dir,
                              shared = list(opts = opts, data_rds = data_rds, seed = opts$seed,
                                            keep_fits = isTRUE(input$run_keep_fits)),
                              core_dir = "R/core", workers = workers, timeout = timeout)
        list(q = q, opts = opts, name_map = NULL)
      } else {
        if (!nzchar(rv$mplus_path %||% "")) stop(tr("mplus_not_found"))
        if (!isTRUE(cfg$mplus_automation)) stop(tr("mplus_need_ma"))
        nm <- mplus_name_map(names(data))
        write_mplus_data(data, file.path(run_dir, "data.dat"))
        utils::write.csv(nm[, c("original", "mplus")], file.path(run_dir, "variable_names.csv"),
                         row.names = FALSE, fileEncoding = "UTF-8")
        codes_found <- numeric(0)
        tasks <- lapply(seq_len(nrow(manifest)), function(i) {
          b <- build_mplus_input(manifest$syntax[i], nm, paste(manifest$id[i], manifest$name[i]))
          inp <- file.path(run_dir, paste0(manifest$id[i], ".inp"))
          writeLines(b$text, inp, useBytes = TRUE)
          if (length(b$missing_codes) > 0) codes_found <<- unique(c(codes_found, b$missing_codes))
          list(id = manifest$id[i], inp_file = normalizePath(inp, winslash = "/"), notes = b$notes)
        })
        if (length(codes_found) > 0) {
          num <- vapply(data, is.numeric, logical(1))
          still <- any(vapply(data[num], function(x) any(x %in% codes_found), logical(1)))
          if (still) {
            msg <- tr("mplus_codes_in_data", paste(codes_found, collapse = ", "))
            notes <- c(notes, msg)
            showNotification(msg, type = "warning", duration = 15)
          }
        }
        q <- create_job_queue(tasks, "mplus", run_dir,
                              shared = list(mplus_path = rv$mplus_path, name_map = nm),
                              core_dir = "R/core", workers = workers, timeout = timeout)
        list(q = q, opts = list(), name_map = nm)
      }
    }, error = function(e) e)
    if (inherits(prep, "error")) {
      return(showNotification(tr("run_prepare_error", conditionMessage(prep)), type = "error", duration = 12))
    }

    state$queue <- prep$q
    rv$batch <- list(
      engine = eng, engine_label = if (eng == "mplus") paste("Mplus", basename(rv$mplus_path)) else "R / lavaan",
      manifest = manifest, opts = prep$opts, notes = notes, run_dir = run_dir,
      started = Sys.time(), name_map = prep$name_map, data = data,
      data_info = rv$data_info, missing_codes = rv$missing_codes,
      n_models = nrow(manifest), versions = version_string(eng), app_version = cfg$version
    )
    rv$results <- list()
    rv$progress <- queue_progress(prep$q)
    rv$log <- character(0)
    rv$running <- TRUE
    showNotification(tr("run_started", nrow(manifest), workers), type = "message")
  })

  # --- Poll -------------------------------------------------------------------
  observe({
    if (!isTRUE(rv$running)) return()
    invalidateLater(700, session)
    q <- state$queue
    p <- tryCatch(queue_poll(q), error = function(e) {
      showNotification(paste("Queue error:", conditionMessage(e)), type = "error")
      NULL
    })
    if (is.null(p)) return()
    old <- isolate(rv$progress)
    if (is.null(old) || p$done != old$done || p$finished) rv$results <- q$results
    rv$progress <- p
    rv$log <- q$log
    if (p$finished) {
      rv$running <- FALSE
      cnt <- status_counts(q$results, q$ids)
      showNotification(tr("run_finished", cnt[["ok"]] + cnt[["warning"]], cnt[["inadmissible"]], cnt[["failed"]]),
                       type = "message", duration = 8)
    }
  })

  observeEvent(input$run_cancel, {
    if (!isTRUE(rv$running) || is.null(state$queue)) return()
    queue_cancel(state$queue)
    rv$results <- state$queue$results
    rv$progress <- queue_progress(state$queue)
    rv$log <- state$queue$log
    rv$running <- FALSE
    showNotification(tr("run_cancelled"), type = "warning")
  })

  output$run_progress <- renderUI({
    p <- rv$progress
    if (is.null(p)) return(NULL)
    cnt <- status_counts(rv$results, rv$batch$manifest$id)
    pct <- if (p$total > 0) round(100 * p$done / p$total) else 0
    fmt_t <- function(s) if (is.na(s)) "—" else if (s < 90) sprintf("%.0f s", s) else sprintf("%.1f min", s / 60)
    tagList(
      div(class = "progress progress-lg",
          div(class = paste("progress-bar", if (isTRUE(rv$running)) "progress-bar-striped active" else "progress-bar-success"),
              role = "progressbar", style = sprintf("width: %d%%;", pct), sprintf("%d%%", pct))),
      div(class = "run-counters",
          span(class = "label label-default", tr("cnt_done", p$done, p$total)),
          span(class = "label label-success", tr("cnt_ok", cnt[["ok"]])),
          span(class = "label label-warning", tr("cnt_warning", cnt[["warning"]])),
          span(class = "label label-warning lbl-inadm", tr("cnt_inadmissible", cnt[["inadmissible"]])),
          span(class = "label label-danger", tr("cnt_failed", cnt[["failed"]])),
          span(class = "label label-info", tr("cnt_time", fmt_t(p$elapsed),
                                               if (isTRUE(rv$running)) fmt_t(p$eta) else "—"))),
      if (!isTRUE(rv$running) && p$done > 0) actionLink("goto_results", tr("run_view_results"))
    )
  })
  observeEvent(input$goto_results, shinydashboard::updateTabItems(session, "tabs", "results"))

  output$run_log <- renderText({
    l <- rv$log
    if (length(l) == 0) return(tr("run_log_empty"))
    paste(utils::tail(l, 200), collapse = "\n")
  })
}

#' Manifest (one row per model) from library entries
build_manifest <- function(models) {
  ph <- unique(unlist(lapply(models, function(m) names(m$assignment))))
  rows <- lapply(models, function(m) {
    a <- as.list(m$assignment)
    vals <- vapply(ph, function(p) if (is.null(a[[p]])) NA_character_ else as.character(a[[p]]), character(1))
    c(list(id = m$id, name = m$name, engine = m$engine, syntax = m$syntax), as.list(vals))
  })
  out <- do.call(rbind.data.frame, c(rows, list(stringsAsFactors = FALSE)))
  names(out) <- c("id", "name", "engine", "syntax", ph)
  rownames(out) <- NULL
  out
}
