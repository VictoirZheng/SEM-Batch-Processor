# Generate simulated data for trying out and testing SEM Batch Processor.
#
#   Rscript dev/make_mock_data.R [output_dir]     (default: dev/mock_data)
#
# The data are simulated from a known population model, so the estimates the
# app reports can be checked against the true values below.
#
#   couples.csv           400 couples, complete data. Covers every built-in template.
#   couples_missing.csv   same, ~6% of values missing, coded -999
#   couples_gbk.csv       same as couples.csv with Chinese column names, GBK encoded
#   couples.xlsx / .sav   same as couples.csv (when openxlsx / haven are installed)
#
# True model (standardised scale, per partner p in H, W; q = the other partner):
#   support_p = -.40 stress_p - .15 stress_q                       (a paths)
#   satis_p   =  .45 support_p + .15 support_q - .20 stress_p      (b, c' paths)
#   commun_p  =  .30 support_p ;  commit_p = .35 satis_p + .20 commun_p
#   attach_p  =  .50 stress_p  (second predictor, correlated with stress)
#   Moderation: the stress_H -> support_H path is weaker when coping is high
#               (interaction -.20), giving a non-zero index of moderated mediation.
#   Items dep1-6, anx1-5, wb1-5 (1-5 Likert) measure three factors with
#   anx -> dep (.50) and dep -> wb (-.45), anx -> wb (-.20).

if (.Platform$OS.type != "windows" && !isTRUE(l10n_info()$`UTF-8`)) {
  invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
}

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) args[[1]] else file.path("dev", "mock_data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(20260101)
n <- 400
z <- function(x) as.numeric(scale(x))

# --- Dyadic variables ------------------------------------------------------
shared <- rnorm(n)                               # couple-level common cause
stress_H <- z(.5 * shared + rnorm(n))
stress_W <- z(.5 * shared + rnorm(n))
coping   <- z(rnorm(n))                          # moderator (husband's coping)

e <- function(sd = 1) rnorm(n, sd = sd)
m_shared <- rnorm(n, sd = .3)
support_H <- -.40 * stress_H - .15 * stress_W + .20 * coping -
  .20 * stress_H * coping + m_shared + e(.8)
support_W <- -.40 * stress_W - .15 * stress_H + m_shared + e(.8)
y_shared <- rnorm(n, sd = .3)
satis_H <- .45 * support_H + .15 * support_W - .20 * stress_H + y_shared + e(.75)
satis_W <- .45 * support_W + .15 * support_H - .20 * stress_W + y_shared + e(.75)

attach_H <- .50 * stress_H + e(.85)
attach_W <- .50 * stress_W + e(.85)
commun_H <- .30 * support_H + e(.9)
commun_W <- .30 * support_W + e(.9)
commit_H <- .35 * satis_H + .20 * commun_H + e(.85)
commit_W <- .35 * satis_W + .20 * commun_W + e(.85)

# --- Covariates ------------------------------------------------------------
age_H     <- round(pmin(pmax(rnorm(n, 38, 8), 22), 65))
age_W     <- round(pmin(pmax(age_H - 2 + rnorm(n, 0, 3), 20), 65))
years_mar <- round(pmin(pmax(age_W - 24 + rnorm(n, 0, 3), 0), 40))
educ_H    <- sample(1:5, n, replace = TRUE, prob = c(.05, .2, .4, .25, .1))
income    <- round(exp(rnorm(n, 9.2, .7)))       # strongly right-skewed
urban     <- rbinom(n, 1, .6)

# --- Likert items for the latent-variable templates -------------------------
f_anx <- z(.4 * stress_H + rnorm(n))
f_dep <- z(.50 * f_anx + rnorm(n, sd = .85))
f_wb  <- z(-.45 * f_dep - .20 * f_anx + rnorm(n, sd = .8))
likert <- function(f, loading) {
  x <- loading * f + rnorm(n, sd = sqrt(1 - loading^2))
  as.integer(cut(x, c(-Inf, -1.2, -.4, .4, 1.2, Inf)))
}
items <- function(prefix, f, loadings) {
  out <- lapply(loadings, function(l) likert(f, l))
  names(out) <- paste0(prefix, seq_along(loadings))
  as.data.frame(out)
}
item_df <- cbind(items("anx", f_anx, c(.80, .75, .70, .65, .60)),
                 items("dep", f_dep, c(.80, .78, .72, .70, .65, .55)),
                 items("wb",  f_wb,  c(.82, .76, .70, .66, .60)))

r2 <- function(x) round(z(x) * 0.8 + 3, 2)        # scale scores around 3, SD .8
couples <- data.frame(
  id = seq_len(n),
  stress_H = r2(stress_H), stress_W = r2(stress_W),
  attach_H = r2(attach_H), attach_W = r2(attach_W),
  support_H = r2(support_H), support_W = r2(support_W),
  commun_H = r2(commun_H), commun_W = r2(commun_W),
  satis_H = r2(satis_H), satis_W = r2(satis_W),
  commit_H = r2(commit_H), commit_W = r2(commit_W),
  coping = r2(coping),
  age_H, age_W, years_mar, educ_H, income, urban,
  item_df
)

write.csv(couples, file.path(out_dir, "couples.csv"), row.names = FALSE)

# --- Missing data coded -999 -------------------------------------------------
miss <- couples
for (v in setdiff(names(miss), "id")) miss[[v]][runif(n) < .06] <- -999
write.csv(miss, file.path(out_dir, "couples_missing.csv"), row.names = FALSE)

# --- Chinese column names, GBK encoding --------------------------------------
zh <- couples
tr <- c(id = "编号", stress = "压力", attach = "依恋焦虑", support = "支持",
        commun = "沟通", satis = "满意度", commit = "承诺", coping = "应对",
        age = "年龄", years_mar = "婚龄", educ = "学历", income = "收入",
        urban = "城市", anx = "焦虑", dep = "抑郁", wb = "幸福感")
new_names <- vapply(names(zh), function(nm) {
  suffix <- sub("^.*?(_H|_W)?$", "\\1", nm, perl = TRUE)
  suffix <- c(`_H` = "_夫", `_W` = "_妻")[suffix]
  if (is.na(suffix)) suffix <- ""
  base <- sub("(_H|_W)$", "", nm)
  digits <- sub("^[a-z]+", "", base)
  stem <- if (base %in% names(tr)) base else sub("[0-9]+$", "", base)
  if (base %in% names(tr)) digits <- ""
  paste0(tr[[stem]], digits, suffix)
}, character(1))
names(zh) <- new_names
con <- file(file.path(out_dir, "couples_gbk.csv"), open = "w", encoding = "GBK")
write.csv(zh, con, row.names = FALSE)
close(con)

if (requireNamespace("openxlsx", quietly = TRUE)) {
  openxlsx::write.xlsx(couples, file.path(out_dir, "couples.xlsx"))
}
if (requireNamespace("haven", quietly = TRUE)) {
  haven::write_sav(couples, file.path(out_dir, "couples.sav"))
}

cat("Wrote", length(list.files(out_dir)), "files to", out_dir, "\n")
print(list.files(out_dir))
