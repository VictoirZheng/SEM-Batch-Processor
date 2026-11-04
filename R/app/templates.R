# Built-in model templates (files in templates/lavaan and templates/mplus).

TEMPLATE_CATALOG <- list(
  mediation = list(en = "Simple mediation (X → M → Y)", zh = "简单中介（X → M → Y）"),
  parallel_mediation = list(en = "Parallel mediation (two mediators)", zh = "并行中介（两个中介变量）"),
  serial_mediation = list(en = "Serial (chain) mediation", zh = "链式中介"),
  moderation = list(en = "Moderation (interaction)", zh = "调节效应（交互项）"),
  moderated_mediation = list(en = "First-stage moderated mediation (Hayes model 7)", zh = "前半路径有调节的中介（Hayes 模型 7）"),
  apim = list(en = "APIM (dyadic data)", zh = "APIM 行动者-同伴互依模型（二元数据）"),
  apim_mediation = list(en = "APIM mediation (APIMeM)", zh = "APIM 中介模型（APIMeM）"),
  cfa = list(en = "Confirmatory factor analysis (3 factors)", zh = "验证性因子分析（3 因子）"),
  latent_mediation = list(en = "Latent-variable mediation (full SEM)", zh = "潜变量中介（完整 SEM）"),
  path_analysis = list(en = "Regression with covariates", zh = "带协变量的回归 / 路径分析")
)

template_file <- function(id, engine) {
  ext <- if (identical(engine, "mplus")) "inp" else "lav"
  file.path("templates", engine, paste0(id, ".", ext))
}

template_choices <- function(engine, lang) {
  ids <- names(TEMPLATE_CATALOG)
  ids <- ids[file.exists(vapply(ids, template_file, character(1), engine = engine))]
  stats::setNames(ids, vapply(ids, function(i) TEMPLATE_CATALOG[[i]][[lang]] %||% i, character(1)))
}

load_template <- function(id, engine) {
  f <- template_file(id, engine)
  if (!file.exists(f)) return("")
  read_text_file(f)
}
