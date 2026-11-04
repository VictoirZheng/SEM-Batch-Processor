test_that("placeholders are parsed in order with types", {
  p <- parse_placeholders("{{Y:outcome}} ~ {{X}} + {{ M }}\n{{X}} ~~ {{X}}\nF =~ {{items:set}}")
  expect_equal(p$table$name, c("Y", "X", "M", "items"))
  expect_equal(p$table$type, c("outcome", "predictor", "mediator", "set"))
  expect_length(p$invalid, 0)
  expect_equal(parse_placeholders("{{1x}} ~ {{a b}}")$invalid, c("{{1x}}", "{{a b}}"))
})

test_that("substitution is strict and handles annotated placeholders", {
  txt <- "{{Y:outcome}} ~ b*{{M}}\n{{M}} ~ a*{{X:predictor}}"
  out <- fill_template(txt, list(Y = "dep", M = "sup", X = "sat"))
  expect_equal(out, "dep ~ b*sup\nsup ~ a*sat")
  # an unassigned placeholder is an error -- never silently dropped (v2 bug)
  expect_error(fill_template(txt, list(Y = "dep", M = "sup")), "X")
  expect_error(fill_template("{{bad name}} ~ x", list()), "Invalid")
})

test_that("combinations: crossing, linking, duplicates, limits", {
  ch <- list(X = c("a", "b"), M = c("m"), Y = c("a", "y"))
  full <- build_combinations(ch, exclude_duplicates = FALSE)
  expect_equal(nrow(full), 4)
  dedup <- build_combinations(ch)
  expect_equal(nrow(dedup), 3)
  expect_equal(attr(dedup, "n_excluded"), 1)

  dy <- list(X_H = c("hs", "ht"), X_W = c("ws", "wt"), Y_H = "hy", Y_W = "wy")
  links <- c(X_H = "X", X_W = "X", Y_H = "Y", Y_W = "Y")
  cmb <- build_combinations(dy, links)
  expect_equal(nrow(cmb), 2)
  expect_equal(cmb$X_W[cmb$X_H == "ht"], "wt")
  expect_equal(count_combinations(dy, links), 2)
  expect_equal(count_combinations(dy), 4)

  expect_error(build_combinations(list(A = c("a", "b"), B = "c"), c(A = "g", B = "g")), "same number")
  expect_error(build_combinations(list(A = letters, B = LETTERS), max_models = 100), "limit")
  expect_error(build_combinations(list(A = character(0))), "at least one")
})

test_that("link groups are suggested for dyadic suffixes only", {
  s <- suggest_links(c("X_H", "X_W", "M_1", "M_2", "item", "Y"))
  expect_equal(unname(s), c("X", "X", "M", "M", "", ""))
})

test_that("item sets are joined per engine", {
  expect_equal(join_set(c("a", "b"), "lavaan"), "a + b")
  expect_equal(join_set(c("a", "b"), "mplus"), "a b")
})

test_that("all built-in templates parse and fill for both engines", {
  for (eng in c("lavaan", "mplus")) {
    for (id in names(TEMPLATE_CATALOG)) {
      txt <- read_text_file(file.path(PROJECT_ROOT, template_file(id, eng)))
      p <- parse_placeholders(txt)
      expect_length(p$invalid, 0)
      expect_gt(nrow(p$table), 0)
      vals <- stats::setNames(as.list(paste0("v", seq_len(nrow(p$table)))), p$table$name)
      expect_false(grepl("{{", fill_template(txt, vals), fixed = TRUE))
    }
  }
})
