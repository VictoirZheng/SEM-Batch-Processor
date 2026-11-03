# Template engine: placeholders, combinations and strict substitution.
#
# Placeholder syntax
#   {{name}}          a placeholder
#   {{name:type}}     same placeholder with a role annotation used only for
#                     grouping in the UI (outcome, predictor, mediator, ...)
# Names start with a letter and may contain letters, digits, "_" and ".".

PH_REGEX <- "\\{\\{\\s*([A-Za-z][A-Za-z0-9_.]*)\\s*(?::\\s*([A-Za-z][A-Za-z0-9_]*)\\s*)?\\}\\}"
PH_ANY_REGEX <- "\\{\\{[^{}]*\\}\\}"

PH_TYPES <- c("outcome", "predictor", "mediator", "moderator", "covariate",
              "latent", "observed", "other", "set")

# A placeholder annotated as {{name:set}} receives a *set* of variables that
# is substituted in one go (e.g. the items of a scale) instead of generating
# one model per variable. The separator depends on the engine.
SET_SEPARATOR <- c(lavaan = " + ", mplus = " ")

#' Guess the role of a placeholder from its name
infer_placeholder_type <- function(name) {
  n <- tolower(name)
  if (grepl("outcome|dependent|^dv|^y($|_|[0-9])", n)) return("outcome")
  if (grepl("mediat|^med|^m($|_|[0-9])", n)) return("mediator")
  if (grepl("moderat|^mod|^w($|_|[0-9])|^z($|_|[0-9])", n)) return("moderator")
  if (grepl("covar|control|^cov", n)) return("covariate")
  if (grepl("predictor|independent|^iv|^x($|_|[0-9])", n)) return("predictor")
  if (grepl("latent|factor|^f($|_|[0-9])|^eta", n)) return("latent")
  if (grepl("item|indicator|observed", n)) return("observed")
  "other"
}

#' Parse the placeholders of a template
#'
#' @return list(table = data.frame(name, type), invalid = character())
#'   `table` has one row per unique placeholder in order of first
#'   appearance. `invalid` lists `{{...}}` tokens that are not valid
#'   placeholders (e.g. `{{ 1x }}` or `{{a b}}`).
parse_placeholders <- function(text) {
  empty <- list(table = data.frame(name = character(0), type = character(0),
                                   stringsAsFactors = FALSE),
                invalid = character(0))
  if (is.null(text) || !nzchar(trimws(text))) return(empty)

  all_tokens <- regmatches(text, gregexpr(PH_ANY_REGEX, text, perl = TRUE))[[1]]
  valid <- grepl(paste0("^", PH_REGEX, "$"), all_tokens, perl = TRUE)
  invalid <- unique(all_tokens[!valid])
  tokens <- all_tokens[valid]
  if (length(tokens) == 0) {
    empty$invalid <- invalid
    return(empty)
  }

  names_ <- sub(paste0("^", PH_REGEX, "$"), "\\1", tokens, perl = TRUE)
  types_ <- sub(paste0("^", PH_REGEX, "$"), "\\2", tokens, perl = TRUE)

  uniq <- unique(names_)
  type_for <- vapply(uniq, function(nm) {
    declared <- types_[names_ == nm & nzchar(types_)]
    if (length(declared) > 0) tolower(declared[1]) else infer_placeholder_type(nm)
  }, character(1))

  list(
    table = data.frame(name = uniq, type = unname(type_for),
                       stringsAsFactors = FALSE),
    invalid = invalid
  )
}

#' Substitute placeholders. Every placeholder must be mapped; nothing is
#' ever silently dropped.
#'
#' @param text template text
#' @param mapping named list/character: placeholder name -> variable name
#' @return the filled text; stops with an informative error otherwise
fill_template <- function(text, mapping) {
  mapping <- unlist(mapping)
  parsed <- parse_placeholders(text)
  if (length(parsed$invalid) > 0) {
    stop("Invalid placeholder(s): ", paste(parsed$invalid, collapse = ", "),
         call. = FALSE)
  }
  needed <- parsed$table$name
  missing_ph <- setdiff(needed, names(mapping)[nzchar(mapping)])
  if (length(missing_ph) > 0) {
    stop("No variable assigned to placeholder(s): ",
         paste0("{{", missing_ph, "}}", collapse = ", "), call. = FALSE)
  }
  if (length(needed) == 0) return(text)

  m <- gregexpr(PH_REGEX, text, perl = TRUE)
  tokens <- regmatches(text, m)[[1]]
  names_ <- sub(paste0("^", PH_REGEX, "$"), "\\1", tokens, perl = TRUE)
  regmatches(text, m) <- list(unname(as.character(mapping[names_])))

  if (grepl("{{", text, fixed = TRUE) || grepl("}}", text, fixed = TRUE)) {
    stop("Unresolved '{{' or '}}' remains after substitution", call. = FALSE)
  }
  text
}

#' Number of combinations implied by choices and link groups (no expansion)
count_combinations <- function(choices, links = NULL) {
  if (length(choices) == 0) return(0)
  lens <- vapply(choices, length, integer(1))
  if (any(lens == 0)) return(0)
  units <- combination_units(names(choices), links)
  prod(vapply(units, function(u) as.numeric(lens[[u[1]]]), numeric(1)))
}

# Internal: group placeholders into independent "units". Placeholders that
# share a non-empty link label form one unit and are paired by position.
combination_units <- function(ph_names, links = NULL) {
  grp <- if (is.null(links)) rep("", length(ph_names)) else {
    g <- unname(as.character(links[ph_names]))
    g[is.na(g)] <- ""
    g
  }
  units <- list()
  seen <- character(0)
  for (i in seq_along(ph_names)) {
    if (!nzchar(grp[i])) {
      units[[length(units) + 1]] <- ph_names[i]
    } else if (!grp[i] %in% seen) {
      seen <- c(seen, grp[i])
      units[[length(units) + 1]] <- ph_names[grp == grp[i]]
    }
  }
  units
}

#' Check link groups for consistency
#' @return character vector of problems (empty when fine)
check_links <- function(choices, links = NULL) {
  if (is.null(links)) return(character(0))
  problems <- character(0)
  units <- combination_units(names(choices), links)
  for (u in units) {
    if (length(u) < 2) next
    lens <- vapply(choices[u], length, integer(1))
    if (length(unique(lens)) > 1) {
      problems <- c(problems, sprintf(
        "Linked placeholders must have the same number of variables: %s",
        paste(sprintf("{{%s}}=%d", u, lens), collapse = ", ")))
    }
  }
  problems
}

#' Build the table of model combinations
#'
#' @param choices named list: placeholder -> character vector of variables
#'   (order matters for linked placeholders)
#' @param links optional named character: placeholder -> link label;
#'   placeholders with the same non-empty label are paired element-wise
#' @param exclude_duplicates drop combinations that assign the same variable
#'   to two different placeholders (e.g. X = M)
#' @param max_models stop if more combinations than this would be created
#' @return data.frame with one character column per placeholder; attribute
#'   "n_excluded" gives the number of dropped duplicate combinations
build_combinations <- function(choices, links = NULL, exclude_duplicates = TRUE,
                               max_models = Inf) {
  if (length(choices) == 0) stop("No placeholders to combine", call. = FALSE)
  empty <- names(choices)[vapply(choices, length, integer(1)) == 0]
  if (length(empty) > 0) {
    stop("Select at least one variable for: ",
         paste0("{{", empty, "}}", collapse = ", "), call. = FALSE)
  }
  problems <- check_links(choices, links)
  if (length(problems) > 0) stop(paste(problems, collapse = "\n"), call. = FALSE)

  total <- count_combinations(choices, links)
  if (total > max_models) {
    stop(sprintf("%s combinations would be generated (limit: %s). Reduce the selection or link paired placeholders.",
                 format(total, big.mark = ","), format(max_models, big.mark = ",")),
         call. = FALSE)
  }

  units <- combination_units(names(choices), links)
  idx <- expand.grid(lapply(units, function(u) seq_along(choices[[u[1]]])),
                     KEEP.OUT.ATTRS = FALSE)
  out <- list()
  for (k in seq_along(units)) {
    for (ph in units[[k]]) out[[ph]] <- choices[[ph]][idx[[k]]]
  }
  combos <- as.data.frame(out[names(choices)], stringsAsFactors = FALSE,
                          check.names = FALSE)

  n_excluded <- 0L
  if (exclude_duplicates && ncol(combos) > 1 && nrow(combos) > 0) {
    dup <- apply(combos, 1, function(r) anyDuplicated(r) > 0)
    n_excluded <- sum(dup)
    combos <- combos[!dup, , drop = FALSE]
  }
  rownames(combos) <- NULL
  attr(combos, "n_excluded") <- n_excluded
  combos
}

#' Human-readable model name from an assignment row
combination_label <- function(assignment) {
  assignment <- unlist(assignment)
  if (length(assignment) == 0) return("")
  paste(sprintf("%s=%s", names(assignment), assignment), collapse = "; ")
}

#' Join the variables of a set placeholder for substitution
join_set <- function(vars, engine = "lavaan") {
  paste(vars, collapse = SET_SEPARATOR[[engine]] %||% " ")
}

#' Suggest link groups for dyadic / paired placeholders
#'
#' Placeholders whose names differ only by a member suffix (_1/_2, _H/_W,
#' _A/_P, _M/_F, _self/_partner, ...) are proposed as one link group so that
#' their variables are paired by position instead of fully crossed.
#' @return named character: placeholder -> suggested group label ("" = none)
suggest_links <- function(ph_names, types = NULL) {
  out <- stats::setNames(rep("", length(ph_names)), ph_names)
  if (length(ph_names) < 2) return(out)
  # letter suffixes need a separator ("X_H"); digits may follow directly ("X1")
  suffix <- "(?i)([_.](1|2|3|h|w|a|p|m|f|self|partner|actor|husband|wife|mother|father|child|parent|t1|t2|t3)|(?<=[a-z])[123])$"
  base <- sub(suffix, "", ph_names, perl = TRUE)
  has_suffix <- base != ph_names & nzchar(base)
  if (!is.null(types)) has_suffix <- has_suffix & types[ph_names] != "set"
  groups <- unique(base[has_suffix])
  for (g in groups) {
    members <- ph_names[has_suffix & base == g]
    if (length(members) >= 2) out[members] <- g
  }
  out
}
