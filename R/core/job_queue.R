# Background execution of model batches.
#
# Models are run in separate R processes (callr) so that the Shiny session
# stays responsive, several models run in parallel, progress is real, and a
# batch can be cancelled. Each worker process receives a small chunk of
# models and writes one result file per model as soon as it is finished;
# the main process polls the result directory.

CORE_WORKER_FILES <- c("utils.R", "lavaan_runner.R", "mplus_runner.R", "job_queue.R")

# ---------------------------------------------------------------------------
# Worker side (runs inside the background process)
# ---------------------------------------------------------------------------

#' Entry point of a worker process
sem_worker_main <- function(core_dir, chunk_file) {
  chunk <- readRDS(chunk_file)
  data <- if (!is.null(chunk$data_rds)) readRDS(chunk$data_rds) else NULL
  for (task in chunk$tasks) {
    file.create(file.path(chunk$result_dir, paste0(task$id, ".started")))
    res <- tryCatch(
      sem_run_task(task, chunk, data),
      error = function(e) {
        r <- new_model_result(task$id, chunk$engine)
        r$errors <- conditionMessage(e)
        r
      }
    )
    save_rds_atomic(res, file.path(chunk$result_dir, paste0(task$id, ".rds")))
  }
  invisible(TRUE)
}

#' Run one task (lavaan fit or Mplus run + parse)
sem_run_task <- function(task, chunk, data) {
  if (identical(chunk$engine, "lavaan")) {
    if (!is.null(chunk$seed)) set.seed(chunk$seed)
    fit_file <- if (!is.null(chunk$fit_dir)) file.path(chunk$fit_dir, paste0(task$id, ".rds")) else NULL
    return(fit_lavaan_model(task$syntax, data, chunk$opts, id = task$id, fit_file = fit_file))
  }
  if (identical(chunk$engine, "mplus")) {
    t0 <- Sys.time()
    status <- tryCatch(run_mplus_process(chunk$mplus_path, task$inp_file, chunk$timeout),
                       error = function(e) conditionMessage(e))
    out_file <- sub("\\.inp$", ".out", task$inp_file, ignore.case = TRUE)
    res <- parse_mplus_output(out_file, task$id, chunk$name_map)
    if (identical(status, "timeout")) {
      res$errors <- c(sprintf("Time limit of %s s exceeded", chunk$timeout), res$errors)
      res$status <- "failed"
    } else if (is.character(status)) {
      res$errors <- c(paste("Could not start Mplus:", status), res$errors)
      res$status <- "failed"
    }
    res$notes <- c(task$notes %||% character(0), res$notes)
    res$elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    return(res)
  }
  stop("Unknown engine: ", chunk$engine)
}

# ---------------------------------------------------------------------------
# Main-process side
# ---------------------------------------------------------------------------

#' Create a job queue
#'
#' @param tasks list of tasks; each has `id` and either `syntax` (lavaan)
#'   or `inp_file` (Mplus)
#' @param engine "lavaan" or "mplus"
#' @param run_dir directory for chunk specifications and result files
#' @param shared list of shared settings: opts, data_rds, seed, mplus_path,
#'   name_map, keep_fits
#' @param core_dir directory containing the core R files
#' @param workers number of parallel worker processes
#' @param timeout per-model time limit in seconds (Inf = none)
create_job_queue <- function(tasks, engine, run_dir, shared = list(),
                             core_dir = "R/core", workers = 1, timeout = Inf,
                             chunk_size = NULL) {
  q <- new.env(parent = emptyenv())
  q$engine <- engine
  q$ids <- vapply(tasks, `[[`, character(1), "id")
  q$tasks <- stats::setNames(tasks, q$ids)
  q$pending <- q$ids
  q$running <- list()
  q$results <- list()
  q$shared <- shared
  q$core_dir <- normalizePath(core_dir, winslash = "/")
  q$workers <- max(1L, as.integer(workers))
  q$timeout <- if (is.null(timeout) || is.na(timeout) || timeout <= 0) Inf else timeout
  q$chunk_size <- chunk_size
  q$run_dir <- run_dir
  q$result_dir <- file.path(run_dir, "results")
  q$fit_dir <- if (isTRUE(shared$keep_fits)) file.path(run_dir, "fits") else NULL
  q$started_at <- Sys.time()
  q$finished_at <- NULL
  q$cancelled <- FALSE
  q$log <- character(0)
  q$chunk_counter <- 0L
  dir.create(q$result_dir, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(q$fit_dir)) dir.create(q$fit_dir, recursive = TRUE, showWarnings = FALSE)
  q
}

queue_log <- function(q, msg) {
  q$log <- c(q$log, sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), msg))
  if (length(q$log) > 500) q$log <- utils::tail(q$log, 500)
}

failed_result <- function(id, engine, msg, status = "failed") {
  r <- new_model_result(id, engine)
  r$errors <- msg
  r$status <- status
  r
}

queue_collect <- function(q, ids) {
  for (id in ids) {
    if (!is.null(q$results[[id]])) next
    f <- file.path(q$result_dir, paste0(id, ".rds"))
    if (file.exists(f)) {
      r <- tryCatch(readRDS(f), error = function(e) NULL)
      if (!is.null(r)) {
        q$results[[id]] <- r
        queue_log(q, sprintf("%s  %s  (%.1f s)%s", id, toupper(r$status), num_or_na(r$elapsed),
                             if (length(r$errors) > 0) paste0("  ", r$errors[1]) else ""))
      }
    }
  }
}

queue_start_worker <- function(q) {
  n_left <- length(q$pending)
  size <- q$chunk_size %||% max(1L, min(10L, ceiling(n_left / (q$workers * 3))))
  ids <- q$pending[seq_len(min(size, n_left))]
  q$pending <- setdiff(q$pending, ids)
  q$chunk_counter <- q$chunk_counter + 1L
  chunk <- c(q$shared, list(
    engine = q$engine,
    tasks = unname(q$tasks[ids]),
    result_dir = q$result_dir,
    fit_dir = q$fit_dir,
    timeout = q$timeout
  ))
  chunk_file <- file.path(q$run_dir, sprintf("chunk_%04d.rds", q$chunk_counter))
  saveRDS(chunk, chunk_file)
  worker <- function(core_dir, files, chunk_file) {
    for (f in files) source(file.path(core_dir, f), local = FALSE)
    sem_worker_main(core_dir, chunk_file)
  }
  proc <- callr::r_bg(worker, args = list(q$core_dir, CORE_WORKER_FILES, chunk_file),
                      supervise = TRUE, package = FALSE)
  q$running[[length(q$running) + 1]] <- list(proc = proc, ids = ids, started = Sys.time())
}

kill_worker <- function(w) {
  tryCatch(w$proc$kill_tree(), error = function(e) tryCatch(w$proc$kill(), error = function(e2) NULL))
}

#' Check workers, collect results and start new workers. Call repeatedly.
#' @return progress list (see queue_progress)
queue_poll <- function(q) {
  keep <- list()
  for (w in q$running) {
    queue_collect(q, w$ids)
    remaining <- w$ids[vapply(w$ids, function(id) is.null(q$results[[id]]), logical(1))]
    alive <- w$proc$is_alive()
    if (!alive) {
      queue_collect(q, remaining)
      remaining <- remaining[vapply(remaining, function(id) is.null(q$results[[id]]), logical(1))]
      if (length(remaining) > 0) {
        msg <- tryCatch({ w$proc$get_result(); "Worker process ended unexpectedly" },
                        error = function(e) paste("Worker process failed:", conditionMessage(e)))
        cur <- remaining[1]
        q$results[[cur]] <- failed_result(cur, q$engine, msg)
        queue_log(q, sprintf("%s  FAILED  %s", cur, msg))
        q$pending <- c(remaining[-1], q$pending)
      }
      next
    }
    if (is.finite(q$timeout) && length(remaining) > 0) {
      marker <- file.path(q$result_dir, paste0(remaining[1], ".started"))
      limit <- if (q$engine == "mplus") q$timeout * 1.2 + 15 else q$timeout
      if (file.exists(marker) &&
          as.numeric(difftime(Sys.time(), file.mtime(marker), units = "secs")) > limit) {
        kill_worker(w)
        cur <- remaining[1]
        q$results[[cur]] <- failed_result(cur, q$engine, sprintf("Time limit of %s s exceeded", q$timeout))
        queue_log(q, sprintf("%s  TIMEOUT", cur))
        q$pending <- c(remaining[-1], q$pending)
        next
      }
    }
    keep[[length(keep) + 1]] <- w
  }
  q$running <- keep
  while (!q$cancelled && length(q$running) < q$workers && length(q$pending) > 0) {
    queue_start_worker(q)
  }
  if (queue_finished(q) && is.null(q$finished_at)) {
    q$finished_at <- Sys.time()
    queue_log(q, "Batch finished")
  }
  queue_progress(q)
}

queue_finished <- function(q) length(q$running) == 0 && length(q$pending) == 0

#' Stop all workers; models not yet finished are marked as cancelled
queue_cancel <- function(q) {
  q$cancelled <- TRUE
  for (w in q$running) {
    queue_collect(q, w$ids)
    kill_worker(w)
  }
  left <- setdiff(q$ids, names(q$results))
  for (id in left) q$results[[id]] <- failed_result(id, q$engine, "Cancelled", status = "cancelled")
  q$running <- list()
  q$pending <- character(0)
  q$finished_at <- Sys.time()
  queue_log(q, sprintf("Cancelled by user (%d model(s) not run)", length(left)))
  invisible(q)
}

queue_progress <- function(q) {
  n_done <- sum(q$ids %in% names(q$results))
  elapsed <- as.numeric(difftime(q$finished_at %||% Sys.time(), q$started_at, units = "secs"))
  eta <- if (n_done > 0 && n_done < length(q$ids)) elapsed / n_done * (length(q$ids) - n_done) else NA_real_
  list(total = length(q$ids), done = n_done, running = length(q$running),
       finished = queue_finished(q), cancelled = q$cancelled,
       elapsed = elapsed, eta = eta)
}

#' Run a queue to completion (for scripts and tests)
run_queue_blocking <- function(q, poll = 0.2, verbose = FALSE) {
  repeat {
    p <- queue_poll(q)
    if (verbose) message(sprintf("%d/%d", p$done, p$total))
    if (p$finished) break
    Sys.sleep(poll)
  }
  q$results
}
