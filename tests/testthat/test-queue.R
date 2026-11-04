test_that("the background queue runs, collects and can be cancelled", {
  skip_if_not_installed("callr")
  d <- make_mediation_data()
  run_dir <- tempfile("run")
  dir.create(run_dir)
  saveRDS(d, file.path(run_dir, "data.rds"))
  tasks <- list(list(id = "M001", syntax = "m1 ~ a*x1\ny1 ~ b*m1\nind := a*b"),
                list(id = "M002", syntax = "m2 ~ a*x2\ny2 ~ b*m2\nind := a*b"),
                list(id = "M003", syntax = "y1 ~ not_a_variable"))
  q <- create_job_queue(tasks, "lavaan", run_dir,
                        shared = list(opts = list(), data_rds = file.path(run_dir, "data.rds"), keep_fits = TRUE),
                        core_dir = file.path(PROJECT_ROOT, "R", "core"), workers = 2, timeout = 120)
  res <- run_queue_blocking(q)
  expect_setequal(names(res), c("M001", "M002", "M003"))
  expect_equal(res$M001$status, "ok")
  expect_equal(res$M003$status, "failed")
  expect_true(file.exists(res$M001$fit_file))
  expect_true(queue_progress(q)$finished)

  q2 <- create_job_queue(tasks[1:2], "lavaan", tempfile("run2"),
                         shared = list(opts = list(bootstrap = 5000), data_rds = file.path(run_dir, "data.rds")),
                         core_dir = file.path(PROJECT_ROOT, "R", "core"), workers = 1)
  queue_poll(q2)
  queue_cancel(q2)
  expect_true(all(vapply(q2$results, `[[`, character(1), "status") %in% c("cancelled", "ok")))
  expect_true(queue_finished(q2))
})
