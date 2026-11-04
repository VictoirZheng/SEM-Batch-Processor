# Locate the project root (tests are run from it or from tests/testthat)
root <- normalizePath(if (file.exists("app.R")) "." else file.path("..", ".."))
for (f in sort(list.files(file.path(root, "R", "core"), pattern = "\\.R$", full.names = TRUE))) {
  source(f, encoding = "UTF-8", local = globalenv())
}
for (f in c("config.R", "i18n.R", "templates.R", "server_run.R")) source(file.path(root, "R", "app", f), encoding = "UTF-8", local = globalenv())
assign("PROJECT_ROOT", root, envir = globalenv())

make_mediation_data <- function(n = 250, seed = 1) {
  set.seed(seed)
  x1 <- rnorm(n); x2 <- rnorm(n)
  m1 <- 0.5 * x1 + rnorm(n); m2 <- rnorm(n)
  y1 <- 0.4 * m1 + 0.1 * x1 + rnorm(n); y2 <- rnorm(n)
  data.frame(x1, x2, m1, m2, y1, y2)
}
