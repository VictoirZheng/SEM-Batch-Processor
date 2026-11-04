# Data page: import, missing codes, derived variables, screening.

server_data <- function(ctx) {
  input <- ctx$input; output <- ctx$output; session <- ctx$session
  rv <- ctx$rv; tr <- ctx$tr

  # Recompute the analysis data from the raw data, missing codes and
  # derived-variable operations (always in this order).
  rebuild_data <- function() {
    req(rv$data_raw)
    rec <- recode_missing(rv$data_raw, rv$missing_codes)
    d <- rec$data
    for (op in rv$derive_ops) {
      d <- switch(op$type,
        center = add_centered(d, op$vars),
        product = add_product(d, op$vars[1], op$vars[2], center = op$center),
        mean = add_mean_score(d, op$name, op$vars, op$min_items))
    }
    rv$missing_counts <- rec$counts
    rv$data <- d
    vars <- names(d)
    updateSelectizeInput(session, "derive_vars", choices = vars[vapply(d, is.numeric, logical(1))])
    updateSelectizeInput(session, "opt_ordered", choices = vars,
                         selected = intersect(isolate(input$opt_ordered), vars))
  }

  read_current_file <- function() {
    f <- input$data_file
    req(f)
    res <- tryCatch(
      read_data_file(f$datapath, f$name, encoding = input$data_encoding %||% "auto",
                     sheet = max(1, as.integer(input$data_sheet %||% 1))),
      error = function(e) {
        showNotification(tr("data_read_error", conditionMessage(e)), type = "error", duration = 10)
        NULL
      })
    if (is.null(res)) return()
    rv$data_raw <- res$data
    rv$data_info <- res$info
    rv$derive_ops <- list()
    rebuild_data()
    msg <- tr("data_loaded", nrow(res$data), ncol(res$data))
    if (nrow(res$info$renamed) > 0) msg <- paste(msg, tr("data_renamed_n", nrow(res$info$renamed)))
    showNotification(msg, type = "message")
    if (length(ctx$rv$library) > 0) showNotification(tr("data_library_warning"), type = "warning", duration = 8)
  }

  observeEvent(input$data_file, read_current_file())
  observeEvent(list(input$data_encoding, input$data_sheet), {
    if (!is.null(input$data_file)) read_current_file()
  }, ignoreInit = TRUE)

  observeEvent(input$apply_missing, {
    codes <- tryCatch(parse_missing_codes(input$missing_codes), error = function(e) {
      showNotification(conditionMessage(e), type = "error"); NULL
    })
    if (is.null(codes)) return()
    rv$missing_codes <- codes
    if (!is.null(rv$data_raw)) {
      rebuild_data()
      showNotification(tr("data_missing_applied", sum(rv$missing_counts)), type = "message")
    }
  })

  output$missing_report <- renderUI({
    cnt <- rv$missing_counts
    if (length(rv$missing_codes) == 0) return(NULL)
    if (length(cnt) == 0) return(div(class = "text-muted small", tr("data_missing_none_found")))
    div(class = "small", tr("data_missing_replaced"),
        tags$ul(lapply(names(cnt), function(v) tags$li(sprintf("%s: %d", v, cnt[[v]])))))
  })

  # --- Derived variables ----------------------------------------------------
  observeEvent(input$derive_add, {
    req(rv$data)
    vars <- input$derive_vars
    type <- input$derive_type
    err <- if (type == "center" && length(vars) < 1) tr("derive_need_vars", 1)
      else if (type == "product" && length(vars) != 2) tr("derive_need_two")
      else if (type == "mean" && length(vars) < 2) tr("derive_need_vars", 2)
      else NULL
    if (!is.null(err)) return(showNotification(err, type = "warning"))
    op <- list(type = type, vars = vars, center = isTRUE(input$derive_center_product),
               name = input$derive_name %||% "score",
               min_items = max(1, min(length(vars), as.integer(input$derive_min_items %||% 1))))
    old <- rv$derive_ops
    rv$derive_ops <- c(old, list(op))
    ok <- tryCatch({ rebuild_data(); TRUE }, error = function(e) {
      showNotification(conditionMessage(e), type = "error"); FALSE
    })
    if (!ok) { rv$derive_ops <- old; rebuild_data() }
  })
  observeEvent(input$derive_clear, {
    rv$derive_ops <- list()
    rebuild_data()
  })
  output$derive_log <- renderUI({
    ops <- rv$derive_ops
    if (length(ops) == 0) return(NULL)
    tagList(
      tags$ul(class = "small", lapply(ops, function(op) tags$li(switch(op$type,
        center = paste(tr("derive_center"), ":", paste(op$vars, collapse = ", ")),
        product = paste(tr("derive_product"), ":", paste(op$vars, collapse = " × "),
                        if (op$center) tr("derive_centered") else ""),
        mean = sprintf("%s: %s = mean(%s)", tr("derive_mean"), op$name, paste(op$vars, collapse = ", ")))))),
      actionLink("derive_clear", tr("derive_clear"))
    )
  })

  # --- Overview and screening -----------------------------------------------
  output$data_summary <- renderUI({
    d <- rv$data
    if (is.null(d)) {
      return(div(class = "callout callout-info", tr("data_none_loaded")))
    }
    o <- data_overview(d)
    fluidRow(
      shinydashboard::valueBox(o$n, tr("data_n"), icon = icon("users"), width = 3, color = "blue"),
      shinydashboard::valueBox(o$p, tr("data_p"), icon = icon("table-columns"), width = 3, color = "blue"),
      shinydashboard::valueBox(sprintf("%.1f%%", o$pct_complete), tr("data_complete"), icon = icon("check"),
                               width = 3, color = if (o$pct_complete < 70) "yellow" else "green"),
      shinydashboard::valueBox(sprintf("%.1f%%", o$pct_missing_cells), tr("data_missing_cells"),
                               icon = icon("circle-question"), width = 3,
                               color = if (o$pct_missing_cells > 10) "yellow" else "green")
    )
  })

  output$data_preview <- DT::renderDT({
    req(rv$data)
    DT::datatable(rv$data, rownames = FALSE, class = "compact stripe nowrap",
                  options = list(scrollX = TRUE, pageLength = 10, language = dt_language(ctx$lang)))
  })

  describe_tbl <- reactive({
    req(rv$data)
    describe_data(rv$data)
  })

  output$data_describe <- DT::renderDT({
    d <- describe_tbl()
    show <- data.frame(
      d$variable, d$type, d$n, sprintf("%.1f%%", d$missing_pct),
      fmt_num(d$mean), fmt_num(d$sd), fmt_num(d$min), fmt_num(d$max),
      fmt_num(d$skew, 2), fmt_num(d$kurtosis, 2),
      flag = vapply(seq_len(nrow(d)), function(i) paste(c(
        if (d$flag_constant[i]) tr("flag_constant"),
        if (d$flag_nonnormal[i]) tr("flag_nonnormal"),
        if (d$flag_missing[i]) tr("flag_missing"),
        if (d$flag_ordinal[i] && !d$flag_constant[i]) tr("flag_ordinal")), collapse = "; "), character(1)),
      stringsAsFactors = FALSE)
    names(show) <- c(tr("col_variable"), tr("col_type"), "n", tr("col_missing"), tr("col_mean"), "SD",
                     "Min", "Max", tr("col_skew"), tr("col_kurt"), tr("col_flag"))
    DT::datatable(show, rownames = FALSE, class = "compact stripe",
                  options = list(pageLength = 25, scrollX = TRUE, language = dt_language(ctx$lang))) |>
      DT::formatStyle(tr("col_flag"), color = "#c0392b", fontWeight = "bold")
  })

  output$missing_plot <- renderPlot({
    d <- describe_tbl()
    d <- d[d$missing > 0, , drop = FALSE]
    if (nrow(d) == 0) {
      graphics::plot.new(); graphics::text(0.5, 0.5, tr("data_no_missing"), cex = 1.3)
      return()
    }
    d <- d[order(d$missing_pct), , drop = FALSE]
    d <- utils::tail(d, 40)
    graphics::par(mar = c(4, max(6, max(nchar(d$variable)) * 0.55), 1, 1))
    graphics::barplot(d$missing_pct, names.arg = d$variable, horiz = TRUE, las = 1,
                      col = "#f0ad4e", border = NA, xlab = tr("col_missing"), cex.names = 0.8)
  })

  output$missing_table <- DT::renderDT({
    req(rv$data)
    d <- rv$data
    pat <- ifelse(is.na(d), "·", "●")
    key <- apply(pat, 1, paste, collapse = "")
    tab <- sort(table(key), decreasing = TRUE)
    out <- data.frame(pattern = names(tab), n = as.integer(tab),
                      pct = sprintf("%.1f%%", 100 * as.integer(tab) / nrow(d)),
                      n_missing = nchar(gsub("●", "", names(tab))), stringsAsFactors = FALSE)
    names(out) <- c(tr("col_pattern"), "n", "%", tr("col_n_missing"))
    DT::datatable(utils::head(out, 50), rownames = FALSE, class = "compact",
                  caption = tr("data_pattern_caption"),
                  options = list(pageLength = 10, dom = "tp", language = dt_language(ctx$lang)))
  })

  output$names_table <- DT::renderDT({
    req(rv$data)
    ren <- rv$data_info$renamed
    nm <- mplus_name_map(names(rv$data))
    out <- data.frame(
      v = nm$original,
      orig = ifelse(nm$original %in% ren$new, ren$original[match(nm$original, ren$new)], ""),
      mplus = ifelse(nm$changed, nm$mplus, ""),
      stringsAsFactors = FALSE)
    names(out) <- c(tr("col_variable"), tr("col_original_name"), tr("col_mplus_name"))
    DT::datatable(out, rownames = FALSE, class = "compact",
                  options = list(pageLength = 25, language = dt_language(ctx$lang)))
  })
}

#' DataTables language settings
dt_language <- function(lang) {
  if (!identical(lang, "zh")) return(list())
  list(search = "搜索：", lengthMenu = "每页 _MENU_ 行",
       info = "第 _START_–_END_ 行，共 _TOTAL_ 行",
       infoEmpty = "无数据", emptyTable = "无数据",
       zeroRecords = "没有匹配的记录",
       infoFiltered = "（从 _MAX_ 行中筛选）",
       paginate = list(previous = "上一页", `next` = "下一页"))
}
