# Results page (fit table, focal effects, model details, export) and plots.

status_label <- function(status, tr) {
  vapply(status, function(s) tr(paste0("st_", s)), character(1), USE.NAMES = FALSE)
}

server_results <- function(ctx) {
  input <- ctx$input; output <- ctx$output
  rv <- ctx$rv; tr <- ctx$tr; lang <- ctx$lang

  manifest <- reactive({
    req(rv$batch)
    rv$batch$manifest
  })
  ph_cols <- reactive(setdiff(names(manifest()), c("id", "name", "engine", "syntax")))

  fit_tbl <- reactive({
    req(length(rv$results) > 0)
    build_fit_table(rv$results, manifest())
  })

  focal_labels <- reactive({
    x <- trimws(unlist(strsplit(input$focal_labels %||% "", "[,;，\\s]+", perl = TRUE)))
    x[nzchar(x)]
  })

  focal_tbl <- reactive({
    req(length(rv$results) > 0)
    build_focal_table(rv$results, manifest(),
                      types = input$focal_types %||% character(0), labels = focal_labels(),
                      method = input$p_adjust %||% "holm", family = input$p_family %||% "all",
                      include_inadmissible = isTRUE(input$focal_inadmissible),
                      alpha = as.numeric(input$alpha %||% 0.05))
  })

  output$results_banner <- renderUI({
    if (is.null(rv$batch)) return(div(class = "callout callout-info", tr("res_none")))
    p <- rv$progress
    if (isTRUE(rv$running)) {
      return(div(class = "callout callout-warning", tr("res_running", p$done, p$total)))
    }
    cnt <- status_counts(rv$results, manifest()$id)
    div(class = "callout callout-success",
        HTML(tr("res_summary", cnt[["total"]], cnt[["ok"]], cnt[["warning"]], cnt[["inadmissible"]], cnt[["failed"]])),
        if (cnt[["inadmissible"]] + cnt[["failed"]] > 0) div(class = "small", tr("res_check_detail")))
  })

  # --- Fit table ----------------------------------------------------------------
  output$fit_table <- DT::renderDT({
    ft <- fit_tbl()
    if (isTRUE(input$fit_only_ok)) ft <- ft[ft$status %in% c("ok", "warning"), , drop = FALSE]
    judge <- fit_judgement(ft$cfi, ft$tli, ft$rmsea, ft$srmr, ft$df)
    out <- data.frame(
      ID = ft$id, Model = ft$name, Status = status_label(ft$status, tr),
      N = ifelse(is.na(ft$n), "—", format(ft$n)),
      chisq = fmt_num(ft$chisq, 2), df = fmt_num(ft$df, 0), p = fmt_p(ft$pvalue),
      CFI = fmt_num(ft$cfi), TLI = fmt_num(ft$tli),
      RMSEA = ifelse(is.na(ft$rmsea), "—", sprintf("%s [%s, %s]", fmt_num(ft$rmsea), fmt_num(ft$rmsea_lower), fmt_num(ft$rmsea_upper))),
      SRMR = fmt_num(ft$srmr), AIC = fmt_num(ft$aic, 1), BIC = fmt_num(ft$bic, 1),
      Fit = ifelse(nzchar(judge), vapply(judge, function(j) if (nzchar(j)) tr(paste0("fj_", j)) else "", character(1)), ""),
      Notes = mapply(function(rob, msg) paste(c(if (rob) tr("res_robust"), if (nzchar(msg)) msg), collapse = "; "),
                     ft$robust, ft$messages, USE.NAMES = FALSE),
      stringsAsFactors = FALSE, check.names = FALSE)
    names(out) <- c("ID", tr("col_model"), tr("col_status"), "N", "χ²", "df", "p", "CFI", "TLI",
                    "RMSEA [90% CI]", "SRMR", "AIC", "BIC", tr("col_fit"), tr("col_notes"))
    DT::datatable(out, rownames = FALSE, class = "compact stripe", escape = TRUE,
                  options = list(pageLength = 25, scrollX = TRUE, language = dt_language(lang),
                                 columnDefs = list(list(targets = 14, render = DT::JS(
                                   "function(d,t){return t==='display'&&d.length>80?'<span title=\"'+d.replace(/\"/g,'&quot;')+'\">'+d.substr(0,80)+'…</span>':d;}"))))) |>
      DT::formatStyle(tr("col_status"), color = DT::styleEqual(
        status_label(c("ok", "warning", "inadmissible", "failed", "cancelled"), tr),
        c("#27ae60", "#d68910", "#ca6f1e", "#c0392b", "#7f8c8d")), fontWeight = "bold")
  })

  # --- Focal effects ------------------------------------------------------------
  output$focal_family_info <- renderUI({
    f <- focal_tbl()
    if (is.null(f)) return(NULL)
    div(class = "small text-muted",
        tr("res_family_size", if (identical(input$p_family, "effect")) paste(range(f$family_size), collapse = "–") else f$family_size[1]))
  })

  output$focal_summary <- DT::renderDT({
    f <- focal_tbl()
    validate(need(!is.null(f), tr("res_no_focal")))
    s <- summarise_focal(f)
    out <- data.frame(s$effect_key, s$n_models, fmt_num(s$median_est), sprintf("[%s, %s]", fmt_num(s$min_est), fmt_num(s$max_est)),
                      fmt_num(s$median_std), sprintf("%.0f%%", s$pct_positive), sprintf("%.0f%%", s$pct_ci_excl_0),
                      sprintf("%.0f%%", s$pct_sig_adj), stringsAsFactors = FALSE)
    names(out) <- c(tr("col_effect"), tr("col_n_models"), tr("col_median"), tr("col_range"), tr("col_median_std"),
                    tr("col_pct_pos"), tr("col_pct_ci"), tr("col_pct_sig"))
    DT::datatable(out, rownames = FALSE, class = "compact", options = list(dom = "t", pageLength = 50, language = dt_language(lang)))
  })

  output$focal_table <- DT::renderDT({
    f <- focal_tbl()
    validate(need(!is.null(f), tr("res_no_focal")))
    ph <- ph_cols()
    out <- data.frame(ID = f$id, f[, ph, drop = FALSE], effect = f$effect,
                      est = fmt_num(f$est), se = fmt_num(f$se),
                      ci = sprintf("[%s, %s]", fmt_num(f$ci_lower), fmt_num(f$ci_upper)),
                      p = fmt_p(f$p), p_adj = fmt_p(f$p_adj),
                      std = fmt_num(f$std_est),
                      sig = ifelse(f$sig_adj, "✔", ""),
                      stringsAsFactors = FALSE, check.names = FALSE)
    names(out)[names(out) %in% c("effect", "est", "se", "ci", "p", "p_adj", "std", "sig")] <-
      c(tr("col_effect"), tr("col_est"), "SE", tr("col_ci"), "p", tr("col_p_adj"), tr("col_std"), tr("col_sig"))
    DT::datatable(out, rownames = FALSE, class = "compact stripe", filter = "top",
                  options = list(pageLength = 25, scrollX = TRUE, language = dt_language(lang)))
  })

  # --- Model detail -------------------------------------------------------------
  observe({
    m <- tryCatch(manifest(), error = function(e) NULL)
    if (is.null(m)) return()
    ch <- stats::setNames(m$id, paste(m$id, m$name))
    sel <- isolate(input$detail_model)
    updateSelectInput(ctx$session, "detail_model", choices = ch,
                      selected = if (!is.null(sel) && sel %in% m$id) sel else m$id[1])
  })

  detail_result <- reactive({
    req(input$detail_model)
    rv$results[[input$detail_model]]
  })

  output$detail_status <- renderUI({
    r <- detail_result()
    if (is.null(r)) return(div(class = "callout callout-info", tr("st_pending")))
    cls <- switch(r$status, ok = "callout-success", warning = "callout-warning",
                  inadmissible = "callout-warning", "callout-danger")
    div(class = paste("callout", cls),
        strong(status_label(r$status, tr)),
        if (!is.na(r$elapsed)) span(class = "small", sprintf(" (%.1f s)", r$elapsed)),
        if (length(r$errors) > 0) div(strong(tr("res_errors")), tags$ul(lapply(r$errors, tags$li))),
        if (length(r$warnings) > 0) div(strong(tr("res_warnings")), tags$ul(lapply(r$warnings, tags$li))),
        if (length(r$notes) > 0) div(strong(tr("res_notes")), tags$ul(lapply(r$notes, tags$li))))
  })

  output$detail_params <- DT::renderDT({
    r <- detail_result()
    validate(need(!is.null(r) && nrow(r$params) > 0, tr("res_no_params")))
    p <- r$params
    if (!isTRUE(input$detail_all_params)) p <- p[!p$op %in% c("~1", "|") & !(p$op == "~~" & p$lhs == p$rhs & is.na(p$p)), , drop = FALSE]
    out <- data.frame(group = p$group, lhs = p$lhs, op = p$op, rhs = p$rhs, label = p$label,
                      est = fmt_num(p$est), se = fmt_num(p$se), z = fmt_num(p$z, 2), p = fmt_p(p$p),
                      ci = ifelse(is.na(p$ci_lower), "—", sprintf("[%s, %s]", fmt_num(p$ci_lower), fmt_num(p$ci_upper))),
                      std = fmt_num(p$std_est),
                      std_ci = ifelse(is.na(p$std_ci_lower), "—", sprintf("[%s, %s]", fmt_num(p$std_ci_lower), fmt_num(p$std_ci_upper))),
                      stringsAsFactors = FALSE)
    if (all(!nzchar(out$group))) out$group <- NULL
    names(out)[names(out) == "ci"] <- tr("col_ci")
    names(out)[names(out) == "std"] <- tr("col_std")
    names(out)[names(out) == "std_ci"] <- tr("col_std_ci")
    DT::datatable(out, rownames = FALSE, class = "compact stripe",
                  options = list(pageLength = 50, scrollX = TRUE, language = dt_language(lang)))
  })

  output$detail_syntax <- renderText({
    req(input$detail_model)
    m <- manifest()
    b <- rv$batch
    if (identical(b$engine, "mplus")) {
      f <- file.path(b$run_dir, paste0(input$detail_model, ".inp"))
      if (file.exists(f)) return(paste(readLines(f, warn = FALSE), collapse = "\n"))
    }
    m$syntax[m$id == input$detail_model]
  })

  output$detail_output <- renderText({
    r <- detail_result()
    if (is.null(r) || length(r$output_text) == 0) return("")
    paste(r$output_text, collapse = "\n")
  })

  # --- Export -------------------------------------------------------------------
  export_info <- reactive({
    b <- rv$batch
    req(b)
    c(b, list(focal_types = input$focal_types, p_adjust = input$p_adjust, family = input$p_family,
              combination_rule = if (length(ph_cols()) > 0) paste(ph_cols(), collapse = ", ") else ""))
  })

  output$methods_table <- renderTable({
    methods_table(export_info(), lang)
  }, striped = TRUE, bordered = TRUE)

  export_sheets <- function() {
    b <- rv$batch
    zh <- identical(lang, "zh")
    ft <- fit_tbl()
    ft$fit <- fit_judgement(ft$cfi, ft$tli, ft$rmsea, ft$srmr, ft$df)
    focal <- focal_tbl()
    sheets <- list(
      methods_table(export_info(), lang),
      b$manifest,
      ft,
      focal,
      summarise_focal(focal),
      build_param_long(rv$results, b$manifest),
      describe_data(b$data)
    )
    names(sheets) <- if (zh) c("方法说明", "模型清单", "拟合指标",
                                "焦点效应", "效应汇总", "全部参数",
                                "数据描述")
      else c("Methods", "Models", "Fit", "Focal effects", "Effect summary", "All parameters", "Descriptives")
    if (!is.null(b$name_map)) sheets[[if (zh) "Mplus变量名" else "Mplus names"]] <- b$name_map
    sheets
  }

  output$dl_xlsx <- downloadHandler(
    filename = function() sprintf("sem_batch_results_%s.xlsx", format(Sys.time(), "%Y%m%d_%H%M")),
    content = function(file) write_results_xlsx(file, export_sheets())
  )
  output$dl_zip <- downloadHandler(
    filename = function() sprintf("sem_batch_%s.zip", format(Sys.time(), "%Y%m%d_%H%M")),
    content = function(file) {
      b <- rv$batch
      x <- tempfile(fileext = ".xlsx")
      write_results_xlsx(x, export_sheets())
      write_results_zip(normalizePath(file, mustWork = FALSE), b$manifest, b$engine, b$data, x,
                        rv$results, b$run_dir, b$opts)
    }
  )
  output$dl_focal_csv <- downloadHandler(
    filename = function() sprintf("focal_effects_%s.csv", format(Sys.time(), "%Y%m%d_%H%M")),
    content = function(file) utils::write.csv(focal_tbl(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  ctx$focal_tbl <- focal_tbl
  ctx$fit_tbl <- fit_tbl
  invisible(NULL)
}

server_plots <- function(ctx) {
  input <- ctx$input; output <- ctx$output; session <- ctx$session
  rv <- ctx$rv; tr <- ctx$tr

  # Focal effects with the default selection (independent of the results tab)
  focal_for_plot <- reactive({
    req(rv$batch, length(rv$results) > 0)
    build_focal_table(rv$results, rv$batch$manifest,
                      types = input$focal_types %||% c("defined", "indirect"),
                      labels = NULL, method = input$p_adjust %||% "holm",
                      family = input$p_family %||% "all", alpha = as.numeric(input$alpha %||% 0.05))
  })

  observe({
    f <- tryCatch(focal_for_plot(), error = function(e) NULL)
    keys <- if (is.null(f)) character(0) else unique(f$effect_key)
    updateSelectInput(session, "plot_effect", choices = keys,
                      selected = if (isolate(input$plot_effect) %in% keys) isolate(input$plot_effect) else keys[1])
  })
  observe({
    req(rv$batch)
    ids <- names(Filter(function(r) !is.null(r$fit_file) && file.exists(r$fit_file), rv$results)) %||% character(0)
    m <- rv$batch$manifest
    ch <- if (length(ids) == 0) character(0) else stats::setNames(ids, paste(ids, m$name[match(ids, m$id)]))
    updateSelectInput(session, "plot_model", choices = ch)
  })

  draw <- function() {
    type <- input$plot_type
    if (is.null(rv$batch) || length(rv$results) == 0) {
      graphics::plot.new(); graphics::text(0.5, 0.5, tr("res_none"), cex = 1.2)
      return(invisible())
    }
    if (type == "spec") {
      f <- focal_for_plot()
      if (is.null(f) || !nzchar(input$plot_effect %||% "")) {
        graphics::plot.new(); graphics::text(0.5, 0.5, tr("res_no_focal"), cex = 1.2)
        return(invisible())
      }
      plot_spec_curve(f, input$plot_effect, standardized = isTRUE(input$plot_std),
                      labels = list(x = tr("plot_x_models"), y = if (isTRUE(input$plot_std)) tr("col_std") else tr("col_est"),
                                    sig = tr("plot_sig"), ns = tr("plot_ns"), title = input$plot_effect))
    } else if (type == "fitdist") {
      ft <- build_fit_table(rv$results, rv$batch$manifest)
      tryCatch(plot_fit_distribution(ft, list(freq = tr("plot_freq"))), error = function(e) {
        graphics::plot.new(); graphics::text(0.5, 0.5, tr("plot_no_fit"), cex = 1.2)
      })
    } else if (type == "path") {
      id <- input$plot_model
      r <- if (!is.null(id) && nzchar(id)) rv$results[[id]] else NULL
      if (is.null(r) || is.null(r$fit_file) || !file.exists(r$fit_file)) {
        graphics::plot.new(); graphics::text(0.5, 0.5, tr("plot_path_unavailable"), cex = 1.1)
        return(invisible())
      }
      fit <- readRDS(r$fit_file)
      draw_path_diagram(fit, title = id)
    }
  }

  output$main_plot <- renderPlot(draw(), res = 96)

  output$dl_plot <- downloadHandler(
    filename = function() sprintf("sem_plot_%s.%s", format(Sys.time(), "%Y%m%d_%H%M%S"), input$plot_format),
    content = function(file) {
      w <- input$plot_w %||% 9; h <- input$plot_h %||% 6
      switch(input$plot_format,
             pdf = grDevices::pdf(file, width = w, height = h),
             svg = grDevices::svg(file, width = w, height = h),
             grDevices::png(file, width = w, height = h, units = "in", res = 300))
      on.exit(grDevices::dev.off())
      draw()
    }
  )
}
