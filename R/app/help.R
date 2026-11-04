# Help & methods page.

help_content <- function(lang) {
  if (identical(lang, "zh")) help_zh() else help_en()
}

help_section <- function(title, ...) {
  shinydashboard::box(title = title, width = 12, status = "primary", collapsible = TRUE, ...)
}

help_zh <- function() {
  fluidRow(column(10, offset = 1,
    help_section("快速上手",
      tags$ol(
        tags$li("在“数据”页上传数据，填写缺失值编码（例如 -999），查看描述统计和缺失模式。"),
        tags$li("在“模型设计”页选择引擎和模板，或自己编写模型；为每个 {{占位符}} 选择一个或多个变量，点击“加入模型库”。"),
        tags$li("在“批量运行”页设置估计方法、缺失处理和 bootstrap，点击“运行全部模型”。运行在后台进行，可随时取消。"),
        tags$li("在“结果”页比较拟合指标，查看焦点效应（含多重比较校正），导出 Excel / ZIP。")
      )),
    help_section("占位符、联动组与题目组",
      p("模板中用 ", code("{{名称}}"), " 标记需要替换的变量。可加角色标注 ", code("{{X:predictor}}"),
        "，只影响界面分组显示。为占位符选择多个变量时，程序会生成所有组合。"),
      p(strong("联动组："), "同一联动组的占位符按位置一一配对，而不是交叉组合。例如 APIM 中 ",
        code("{{X_H}}"), " 与 ", code("{{X_W}}"), " 分别选择 [丈夫满意度, 丈夫压力] 和 [妻子满意度, 妻子压力]，",
        "联动后只生成“满意度-满意度”和“压力-压力”两种配对。名称以 _1/_2、_H/_W、_A/_P 等结尾的占位符会自动建议联动。"),
      p(strong("题目组："), code("{{F1_items:set}}"), " 一次代入一组变量（lavaan 用 + 连接，Mplus 用空格连接），适合量表题目。"),
      p("默认跳过同一变量出现在两个位置的组合（如 X = M）。替换是严格的：任何未指定的占位符都会报错，不会悄悄删除模型中的语句。")),
    help_section("统计方法说明",
      tags$ul(
        tags$li(strong("间接效应："), "a·b 的抽样分布通常偏态，建议使用 bootstrap 置信区间（2000–5000 次）。",
                "百分位法（percentile）是默认方法；偏差校正法（bias-corrected，与 Mplus 的 BCBOOTSTRAP 相同）在小样本中第一类错误率可能偏高（Fritz, Taylor & MacKinnon, 2012）。BCa 需要 bootstrap 次数多于样本量，否则自动改用偏差校正法并注明。lavaan 的 bootstrap 只支持 ML 估计。"),
        tags$li(strong("缺失数据："), "默认使用 FIML。外生预测变量的缺失默认按列删除处理（与 Mplus 默认一致），选择“FIML（含外生预测变量）”可将其纳入似然。WLSMV 不支持 FIML，会自动改为成对删除。"),
        tags$li(strong("非正态数据："), "偏度或峰度较大时，可用 MLR（稳健标准误与校正 χ²）或 bootstrap。稳健估计时，表格中报告校正 χ² 和稳健 CFI/TLI/RMSEA。"),
        tags$li(strong("拟合指标："), "“良好 / 可接受”依据 Hu & Bentler (1999) 的经验标准，只是参考，并非检验。df = 0 的饱和模型必然完美拟合，其拟合指标没有信息量。"),
        tags$li(strong("可接受性："), "未收敛的模型标为“失败”。出现负方差（Heywood case）、标准化载荷或相关的绝对值大于 1、矩阵非正定的模型标为“不可接受解”，默认不纳入焦点效应汇总。"),
        tags$li(strong("多重比较："), "对大量变量组合逐一检验会增加假阳性（“分叉路径”问题）。结果页对焦点效应进行 Holm（默认，控制族错误率）、Benjamini–Hochberg（控制错误发现率）或 Bonferroni 校正。校正族可以是整批全部焦点检验，也可以是每种效应分别校正。一批中有多个模板时，效应按模板区分（[T1]、[T2] …）。规格曲线图把各模型的估计值按大小排列，下方标出每个模型用了哪些变量，可以看出结论依赖于哪些选择。建议报告全部模型及所用的校正方法。"),
        tags$li(strong("可复现性："), "导出的 Excel 包含全部设置、随机种子和软件版本；ZIP 包含每个模型的语法与输出、分析数据，以及可复现 lavaan 分析的 reproduce.R。")
      )),
    help_section("线上版与本地版",
      p("线上版（如 shinyapps.io）只使用开源的 lavaan，并限制模型数量和 bootstrap 次数，以保护共享服务器。数据只保存在会话临时目录中，关闭页面后即删除。出于安全考虑，模型语法中只允许 lavaan 的修饰函数（如 start()、equal()）和算术函数（如 exp()、sqrt()）。"),
      p("本地版在您的电脑上运行（", code("shiny::runApp()"), "）。如果检测到 Mplus，并已安装 R 包 MplusAutomation，即可用 Mplus 引擎。",
        "程序会自动生成 TITLE、DATA、NAMES 和 MISSING = .，把超过 8 个字符的变量名映射为 Mplus 短名（结果中自动还原），并对超过 90 个字符的行自动折行。"),
      p("运行模式会自动识别，也可以用环境变量 ", code("SEMBP_MODE=online|local"), " 指定。")),
    help_section("参考文献",
      tags$ul(
        tags$li("Fritz, M. S., Taylor, A. B., & MacKinnon, D. P. (2012). Explanation of two anomalous results in statistical mediation analysis. Multivariate Behavioral Research, 47(1), 61–87."),
        tags$li("Hayes, A. F. (2015). An index and test of linear moderated mediation. Multivariate Behavioral Research, 50(1), 1–22."),
        tags$li("Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis. Structural Equation Modeling, 6(1), 1–55."),
        tags$li("Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. Guilford Press."),
        tags$li("Rhemtulla, M., Brosseau-Liard, P. É., & Savalei, V. (2012). When can categorical variables be treated as continuous? A comparison of robust continuous and categorical SEM estimation methods under suboptimal conditions. Psychological Methods, 17(3), 354–373."),
        tags$li("Rosseel, Y. (2012). lavaan: An R package for structural equation modeling. Journal of Statistical Software, 48(2), 1–36."),
        tags$li("Simonsohn, U., Simmons, J. P., & Nelson, L. D. (2020). Specification curve analysis. Nature Human Behaviour, 4, 1208–1214."),
        tags$li("West, S. G., Finch, J. F., & Curran, P. J. (1995). Structural equation models with nonnormal variables. In R. H. Hoyle (Ed.), Structural equation modeling (pp. 56–75). Sage.")
      )),
    help_section("联系",
      p("作者：Qingguang \"Victor\" Zheng · zhengqg@mail.bnu.edu.cn · ",
        tags$a(href = "https://github.com/VictoirZheng/SEM-Batch-Processor", target = "_blank", "GitHub")))
  ))
}

help_en <- function() {
  fluidRow(column(10, offset = 1,
    help_section("Quick start",
      tags$ol(
        tags$li("Data: upload your data, declare missing-value codes (e.g. -999), check descriptives and missing-data patterns."),
        tags$li("Models: choose the engine and a template or write your own; select one or more variables for each {{placeholder}} and add the combinations to the model library."),
        tags$li("Run: choose the estimator, missing-data handling and bootstrap, then run all models. Models run in the background and can be cancelled."),
        tags$li("Results: compare fit, inspect focal effects with multiple-comparison correction, export Excel / ZIP.")
      )),
    help_section("Placeholders, link groups and item sets",
      p("Mark variables to be substituted with ", code("{{name}}"), ". An optional role annotation ", code("{{X:predictor}}"),
        " only affects how the placeholder is displayed. Selecting several variables for a placeholder generates every combination."),
      p(strong("Link groups: "), "placeholders in the same link group are paired by position instead of crossed. In an APIM, selecting [H_sat, H_stress] for ",
        code("{{X_H}}"), " and [W_sat, W_stress] for ", code("{{X_W}}"), " yields only the sat–sat and stress–stress pairs. Placeholders ending in _1/_2, _H/_W, _A/_P, … are linked automatically (you can change this)."),
      p(strong("Item sets: "), code("{{F1_items:set}}"), " substitutes a whole set of variables at once (joined with + for lavaan and spaces for Mplus), e.g. the items of a scale."),
      p("Combinations that put the same variable in two places (e.g. X = M) are skipped by default. Substitution is strict: an unassigned placeholder is an error; statements are never silently removed.")),
    help_section("Statistical notes",
      tags$ul(
        tags$li(strong("Indirect effects: "), "the sampling distribution of a·b is usually skewed, so bootstrap confidence intervals (2000–5000 draws) are recommended. Percentile intervals are the default; bias-corrected intervals (Mplus BCBOOTSTRAP) can have inflated Type I error in small samples (Fritz, Taylor & MacKinnon, 2012). BCa needs more bootstrap draws than cases; otherwise bias-corrected intervals are used and this is noted. lavaan bootstraps only under ML."),
        tags$li(strong("Missing data: "), "FIML is the default. Missing values on exogenous predictors lead to listwise deletion of those cases (as in Mplus) unless “FIML incl. exogenous predictors” is chosen. FIML is not available with WLSMV; pairwise deletion is used instead."),
        tags$li(strong("Non-normal data: "), "use MLR (robust SEs and scaled χ²) or bootstrap. With robust estimators the tables show the scaled χ² and robust CFI/TLI/RMSEA."),
        tags$li(strong("Fit indices: "), "the good/acceptable labels follow the Hu & Bentler (1999) rules of thumb; they are heuristics, not tests. Saturated models (df = 0) fit perfectly by construction and their fit indices carry no information."),
        tags$li(strong("Admissibility: "), "non-converged models are marked as failed. Solutions with negative variances (Heywood cases), standardized loadings or correlations beyond ±1 or non-positive-definite matrices are flagged as inadmissible and excluded from the focal-effect summary by default."),
        tags$li(strong("Multiple comparisons: "), "testing many variable combinations inflates false positives (the garden of forking paths). Focal effects are corrected with Holm (default; family-wise error rate), Benjamini–Hochberg (false discovery rate) or Bonferroni, either over all focal tests of the batch or separately per effect. When a batch contains several templates, effects are kept apart per template ([T1], [T2], …). The specification curve sorts the estimates of all models and shows below them which variables each model used, so you can see which choices drive the result. Report all models and the correction used."),
        tags$li(strong("Reproducibility: "), "the Excel export records all settings, the random seed and software versions; the ZIP contains every model's syntax and output, the analysed data and a reproduce.R script for lavaan.")
      )),
    help_section("Online and local versions",
      p("The online version (e.g. shinyapps.io) uses the open-source lavaan engine only and limits the number of models and bootstrap draws to protect the shared server. Data are kept in a temporary session folder and deleted when the page is closed. For security, model syntax may only call lavaan modifiers (e.g. start(), equal()) and arithmetic functions (e.g. exp(), sqrt())."),
      p("The local version runs on your computer (", code("shiny::runApp()"), "). If Mplus is found and the R package MplusAutomation is installed, the Mplus engine becomes available. The program writes TITLE, DATA, NAMES and MISSING = . itself, maps names longer than 8 characters to short Mplus names (and back in the results) and wraps lines longer than 90 characters."),
      p("The mode is detected automatically and can be set with the environment variable ", code("SEMBP_MODE=online|local"), ".")),
    help_section("References",
      tags$ul(
        tags$li("Fritz, M. S., Taylor, A. B., & MacKinnon, D. P. (2012). Explanation of two anomalous results in statistical mediation analysis. Multivariate Behavioral Research, 47(1), 61–87."),
        tags$li("Hayes, A. F. (2015). An index and test of linear moderated mediation. Multivariate Behavioral Research, 50(1), 1–22."),
        tags$li("Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis. Structural Equation Modeling, 6(1), 1–55."),
        tags$li("Kenny, D. A., Kashy, D. A., & Cook, W. L. (2006). Dyadic data analysis. Guilford Press."),
        tags$li("Rhemtulla, M., Brosseau-Liard, P. É., & Savalei, V. (2012). When can categorical variables be treated as continuous? A comparison of robust continuous and categorical SEM estimation methods under suboptimal conditions. Psychological Methods, 17(3), 354–373."),
        tags$li("Rosseel, Y. (2012). lavaan: An R package for structural equation modeling. Journal of Statistical Software, 48(2), 1–36."),
        tags$li("Simonsohn, U., Simmons, J. P., & Nelson, L. D. (2020). Specification curve analysis. Nature Human Behaviour, 4, 1208–1214."),
        tags$li("West, S. G., Finch, J. F., & Curran, P. J. (1995). Structural equation models with nonnormal variables. In R. H. Hoyle (Ed.), Structural equation modeling (pp. 56–75). Sage.")
      )),
    help_section("Contact",
      p("Author: Qingguang \"Victor\" Zheng · zhengqg@mail.bnu.edu.cn · ",
        tags$a(href = "https://github.com/VictoirZheng/SEM-Batch-Processor", target = "_blank", "GitHub")))
  ))
}
