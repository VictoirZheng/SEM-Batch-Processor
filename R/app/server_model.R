# Model page: engine, template, placeholder assignment, model library.

ph_input_id <- function(prefix, name) paste0(prefix, gsub("[^A-Za-z0-9_]", "_", name))

server_model <- function(ctx) {
  input <- ctx$input; output <- ctx$output; session <- ctx$session
  rv <- ctx$rv; tr <- ctx$tr; lang <- ctx$lang; cfg <- ctx$cfg

  mplus_ok <- reactive(identical(cfg$mode, "local") && nzchar(rv$mplus_path %||% ""))
  engine <- reactive(input$engine %||% "lavaan")

  # --- Engine & template ------------------------------------------------------
  observeEvent(input$engine, {
    if (identical(input$engine, "mplus") && !mplus_ok()) {
      showNotification(if (cfg$mode == "online") tr("mplus_online_only") else tr("mplus_not_found"),
                       type = "warning", duration = 8)
      updateRadioButtons(session, "engine", selected = "lavaan")
      return()
    }
    updateSelectInput(session, "template_select", choices = template_choices(engine(), lang))
    if (nzchar(trimws(isolate(input$template_text) %||% ""))) {
      showNotification(tr("engine_switched_reload"), type = "message", duration = 6)
    }
  })

  output$engine_note <- renderUI({
    if (mplus_ok()) return(NULL)
    div(class = "small text-muted",
        if (cfg$mode == "online") tr("mplus_online_only") else tr("mplus_not_found"))
  })

  observeEvent(input$load_template, {
    req(input$template_select)
    txt <- load_template(input$template_select, engine())
    updateTextAreaInput(session, "template_text", value = txt)
  })

  # --- Placeholders -----------------------------------------------------------
  template_d <- debounce(reactive(input$template_text %||% ""), 600)
  parsed <- reactive(parse_placeholders(template_d()))

  # Re-render the assignment widgets only when the set of placeholders or
  # the variable list changes (a reactiveVal does not fire on identical values).
  ph_sig <- reactiveVal(NULL)
  observe({
    p <- parsed()$table
    sig <- list(names = p$name, types = p$type, vars = names(rv$data))
    if (!identical(sig, isolate(ph_sig()))) ph_sig(sig)
  })

  output$placeholder_ui <- renderUI({
    sig <- ph_sig()
    p <- isolate(parsed()$table)
    if (is.null(sig) || nrow(p) == 0) {
      return(div(class = "text-muted", HTML(tr("ph_none"))))
    }
    vars <- sig$vars
    if (is.null(vars)) return(div(class = "callout callout-warning", tr("ph_need_data")))
    links <- suggest_links(p$name, stats::setNames(p$type, p$name))
    link_choices <- c(stats::setNames("", tr("link_none")),
                      unique(c(links[nzchar(links)], LETTERS[1:6])))
    tagList(
      div(class = "small text-muted ph-help", HTML(tr("ph_help"))),
      lapply(seq_len(nrow(p)), function(i) {
        nm <- p$name[i]; type <- p$type[i]
        sel_id <- ph_input_id("ph_", nm)
        link_id <- ph_input_id("link_", nm)
        is_set <- identical(type, "set")
        div(class = "ph-row",
            div(class = "ph-head",
                tags$code(sprintf("{{%s}}", nm)),
                tags$span(class = paste("label ph-type", paste0("ph-", type)), tr(paste0("type_", type)))),
            fluidRow(
              column(if (is_set) 12 else 8,
                     selectizeInput(sel_id, NULL, choices = vars, multiple = TRUE,
                                    selected = intersect(isolate(input[[sel_id]]), vars),
                                    options = list(plugins = list("remove_button"),
                                                   placeholder = if (is_set) tr("ph_pick_set") else tr("ph_pick")))),
              if (!is_set) column(4, selectInput(link_id, NULL, link_choices,
                                                 selected = isolate(input[[link_id]]) %||% links[[nm]]))
            ))
      })
    )
  })

  # Current choices: placeholder -> character vector of candidate values
  ph_state <- reactive({
    p <- parsed()$table
    eng <- engine()
    choices <- list(); links <- character(0); sets <- character(0); empty <- character(0)
    for (i in seq_len(nrow(p))) {
      nm <- p$name[i]
      sel <- input[[ph_input_id("ph_", nm)]] %||% character(0)
      if (length(sel) == 0) empty <- c(empty, nm)
      if (identical(p$type[i], "set")) {
        choices[[nm]] <- if (length(sel) > 0) join_set(sel, eng) else character(0)
        sets <- c(sets, nm)
      } else {
        choices[[nm]] <- sel
        links[nm] <- input[[ph_input_id("link_", nm)]] %||% ""
      }
    }
    list(table = p, choices = choices, links = links, sets = sets, empty = empty)
  })

  output$combo_info <- renderUI({
    st <- ph_state()
    if (nrow(st$table) == 0) {
      if (nzchar(trimws(template_d()))) return(div(class = "callout callout-info", tr("combo_single")))
      return(NULL)
    }
    if (length(st$empty) > 0) {
      return(div(class = "callout callout-warning",
                 tr("combo_missing", paste0("{{", st$empty, "}}", collapse = ", "))))
    }
    probs <- check_links(st$choices, st$links)
    if (length(probs) > 0) return(div(class = "callout callout-danger", tr("combo_link_error"), tags$br(), probs))
    n <- count_combinations(st$choices, st$links)
    full <- prod(vapply(st$choices, length, integer(1)))
    lim <- cfg$limit$max_models - length(rv$library)
    div(class = paste("callout", if (n > lim) "callout-danger" else "callout-success"),
        HTML(tr("combo_count", format(n, big.mark = ","))),
        if (full > n) div(class = "small", tr("combo_linked_saving", format(full, big.mark = ","))),
        if (isTRUE(input$exclude_duplicates)) div(class = "small", tr("combo_dup_note")),
        if (n > lim) div(tr("combo_over_limit", lim)))
  })

  # Combination table (data.frame of assignments)
  build_combo_table <- function() {
    st <- ph_state()
    if (nrow(st$table) == 0) return(data.frame(row.names = 1L))  # one model, no placeholders
    build_combinations(st$choices, st$links, exclude_duplicates = isTRUE(input$exclude_duplicates),
                       max_models = cfg$limit$max_models - length(rv$library))
  }

  # Display label for an assignment (sets are summarised)
  assignment_label <- function(a, sets) {
    a <- unlist(a)
    if (length(a) == 0) return("")
    for (s in intersect(names(a), sets)) {
      n_items <- length(strsplit(a[[s]], if (engine() == "mplus") " " else " + ", fixed = TRUE)[[1]])
      a[[s]] <- sprintf("[%d %s]", n_items, tr("items"))
    }
    combination_label(a)
  }

  # Syntax for one assignment, validated
  make_syntax <- function(template, a) fill_template(template, as.list(a))

  output$template_check <- renderUI({
    txt <- template_d()
    if (!nzchar(trimws(txt))) return(NULL)
    p <- parsed()
    msgs <- list()
    if (length(p$invalid) > 0) {
      msgs <- c(msgs, list(div(class = "callout callout-danger", tr("check_invalid_ph", paste(p$invalid, collapse = ", ")))))
    }
    st <- ph_state()
    if (is.null(rv$data) || length(st$empty) > 0 || length(p$invalid) > 0) {
      if (length(msgs) == 0 && nrow(p$table) > 0) {
        msgs <- list(div(class = "small text-muted", tr("check_after_assign")))
      }
      return(tagList(msgs))
    }
    first <- tryCatch({
      cmb <- build_combo_table()
      if (nrow(cmb) == 0) stop(tr("combo_all_excluded"))
      make_syntax(txt, if (ncol(cmb) > 0) cmb[1, , drop = FALSE] else list())
    }, error = function(e) e)
    if (inherits(first, "error")) {
      return(tagList(msgs, div(class = "callout callout-danger", conditionMessage(first))))
    }
    if (engine() == "lavaan") {
      chk <- check_lavaan_syntax(first, names(rv$data))
      if (chk$ok) {
        msgs <- c(msgs, list(div(class = "callout callout-success",
                                 tr("check_lavaan_ok", length(chk$observed), length(chk$latent)))))
      } else {
        msgs <- c(msgs, list(div(class = "callout callout-danger", tr("check_first_model"), tags$br(),
                                 paste(chk$errors, collapse = "; "))))
      }
    } else {
      b <- build_mplus_input(first, mplus_name_map(names(rv$data)), "check")
      cls <- if (length(b$notes) == 0) "callout-success" else "callout-warning"
      msgs <- c(msgs, list(div(class = paste("callout", cls),
                               if (length(b$notes) == 0) tr("check_mplus_ok") else tagList(
                                 tr("check_mplus_notes"), tags$ul(lapply(b$notes, tags$li))))))
    }
    tagList(msgs)
  })

  observeEvent(input$preview_model, {
    txt <- input$template_text %||% ""
    if (!nzchar(trimws(txt))) return(showNotification(tr("model_no_template"), type = "warning"))
    res <- tryCatch({
      cmb <- build_combo_table()
      if (nrow(cmb) == 0) stop(tr("combo_all_excluded"))
      a <- if (ncol(cmb) > 0) cmb[1, , drop = FALSE] else list()
      syn <- make_syntax(txt, a)
      if (engine() == "mplus") {
        req(rv$data)
        b <- build_mplus_input(syn, mplus_name_map(names(rv$data)),
                               paste("M001", assignment_label(a, ph_state()$sets)))
        list(text = b$text, notes = b$notes)
      } else list(text = syn, notes = character(0))
    }, error = function(e) e)
    if (inherits(res, "error")) return(showNotification(conditionMessage(res), type = "error", duration = 8))
    showModal(modalDialog(
      title = tr("model_preview_title"), size = "l", easyClose = TRUE,
      if (length(res$notes) > 0) div(class = "callout callout-warning", tags$ul(lapply(res$notes, tags$li))),
      tags$pre(class = "syntax", res$text),
      footer = modalButton(tr("close"))))
  })

  observeEvent(input$add_models, {
    txt <- input$template_text %||% ""
    if (!nzchar(trimws(txt))) return(showNotification(tr("model_no_template"), type = "warning"))
    if (is.null(rv$data)) return(showNotification(tr("ph_need_data"), type = "warning"))
    st <- ph_state()
    res <- tryCatch({
      cmb <- build_combo_table()
      if (nrow(cmb) == 0) stop(tr("combo_all_excluded"))
      syn <- lapply(seq_len(nrow(cmb)), function(i) make_syntax(txt, if (ncol(cmb) > 0) cmb[i, , drop = FALSE] else list()))
      if (engine() == "lavaan") {
        chk <- check_lavaan_syntax(syn[[1]], names(rv$data))
        if (!chk$ok) stop(paste(chk$errors, collapse = "; "))
      }
      list(cmb = cmb, syn = syn)
    }, error = function(e) e)
    if (inherits(res, "error")) return(showNotification(conditionMessage(res), type = "error", duration = 10))

    n <- length(res$syn)
    ids <- make_model_ids(n, start = rv$next_model)
    prefix <- trimws(input$model_prefix %||% "")
    new_models <- lapply(seq_len(n), function(i) {
      a <- if (ncol(res$cmb) > 0) unlist(res$cmb[i, , drop = FALSE]) else character(0)
      lbl <- assignment_label(a, st$sets)
      list(id = ids[i],
           name = paste0(if (nzchar(prefix)) paste0(prefix, " ") else "", if (nzchar(lbl)) lbl else tr("model_single")),
           engine = engine(), syntax = res$syn[[i]], template = txt, assignment = a,
           created = Sys.time())
    })
    names(new_models) <- ids
    rv$library <- c(rv$library, new_models)
    rv$next_model <- rv$next_model + n
    excl <- attr(res$cmb, "n_excluded") %||% 0
    showNotification(tr("model_added", n, if (excl > 0) tr("model_excluded", excl) else ""),
                     type = "message", duration = 6)
  })

  # --- Library ----------------------------------------------------------------
  library_df <- reactive({
    lib <- rv$library
    if (length(lib) == 0) return(data.frame(id = character(0), name = character(0), engine = character(0)))
    data.frame(id = names(lib),
               name = vapply(lib, `[[`, character(1), "name"),
               engine = vapply(lib, `[[`, character(1), "engine"),
               stringsAsFactors = FALSE)
  })

  output$library_info <- renderUI({
    d <- library_df()
    if (nrow(d) == 0) return(div(class = "text-muted", tr("library_empty")))
    other <- sum(d$engine != engine())
    div(tr("library_count", nrow(d)),
        if (other > 0) div(class = "callout callout-warning", tr("library_engine_mismatch", other)))
  })

  output$library_table <- DT::renderDT({
    d <- library_df()
    names(d) <- c("ID", tr("col_model"), tr("engine"))
    DT::datatable(d, rownames = FALSE, selection = "multiple", class = "compact stripe",
                  options = list(pageLength = 10, scrollX = TRUE, language = dt_language(lang)))
  })

  selected_ids <- function() {
    s <- input$library_table_rows_selected
    d <- library_df()
    if (length(s) == 0) character(0) else d$id[s]
  }

  observeEvent(input$library_view, {
    ids <- selected_ids()
    if (length(ids) == 0) ids <- utils::head(names(rv$library), 1)
    if (length(ids) == 0) return()
    m <- rv$library[[ids[1]]]
    showModal(modalDialog(title = paste(m$id, m$name), size = "l", easyClose = TRUE,
                          tags$pre(class = "syntax", m$syntax), footer = modalButton(tr("close"))))
  })
  observeEvent(input$library_remove, {
    ids <- selected_ids()
    if (length(ids) == 0) return(showNotification(tr("library_select_first"), type = "warning"))
    rv$library <- rv$library[setdiff(names(rv$library), ids)]
    showNotification(tr("library_removed", length(ids)), type = "message")
  })
  observeEvent(input$library_clear, {
    if (length(rv$library) == 0) return()
    showModal(modalDialog(title = tr("library_clear"), tr("library_clear_confirm", length(rv$library)),
                          footer = tagList(modalButton(tr("cancel")),
                                           actionButton("library_clear_ok", tr("library_clear"), class = "btn-danger"))))
  })
  observeEvent(input$library_clear_ok, {
    rv$library <- list()
    rv$next_model <- 1L
    removeModal()
  })
}
