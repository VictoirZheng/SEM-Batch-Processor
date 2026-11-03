# 代码审查与重构记录（v3.0.0）

本文档记录对 v2.1.x 的完整审查结果，以及 v3.0.0 中对应的处理方式。
条目按严重程度排序：**A 类 = 会导致结果错误或功能无法使用**；**B 类 = 统计方法问题**；
**C 类 = 界面与交互**；**D 类 = 工程与可维护性**。

---

## A 类：结果错误 / 功能不可用

| # | 位置 (v2.1.x) | 问题 | v3.0.0 处理 |
|---|---|---|---|
| A1 | 全部 37 个 `.R` / 模板文件 | 文件开头带 UTF-8 BOM，`source()` 在 Linux（含 shinyapps.io）上报 `unexpected input`，应用无法启动 | 去除全部 BOM；新增测试保证源码无 BOM |
| A2 | `app.R` | 启动时对缺失包调用 `install.packages()`；`shinyWidgets`、`plotly`、`dplyr` 等被声明为必需但从未使用，联网受限时直接启动失败 | 只加载真正使用的包；缺包时给出明确提示，不在运行时安装 |
| A3 | `model_library.R::generate_combinations` | 替换后**静默删除仍含 `{{…}}` 的行**。`{{outcome:Y}}` 这类带类型标注的占位符永远匹配不上，对应的回归方程被悄悄删掉，跑出来的是另一个模型 | 严格替换：任何未替换的占位符都会报错并阻止生成，从不删除用户代码 |
| A4 | `mplus_batch_processor.R` / 模板 | `prepareMplusData` 把 NA 写成 `.`，但模板写的是 `MISSING ARE ALL (-999)`，且从未声明 `MISSING = .`。Mplus 要么读数报错，要么错误处理缺失值 | 自行写 `.dat`（缺失统一为 `.`），由程序生成 `MISSING = .;`，并移除模板中冲突的 `MISSING`/`NAMES`/`DATA`/`TITLE` 语句（附警告） |
| A5 | `run_mplus_batch`（"开始批处理"按钮的 Mplus 路径） | 生成 inp 时**没有写 `NAMES`**，Mplus 必然报错；只有两步法路径会写 | 统一为一个 inp 生成函数 |
| A6 | Mplus 变量名 | 没有处理 Mplus 的 8 字符变量名限制、大小写不敏感、非法字符；也没有处理 90 字符行宽（`USEVARIABLES` 很容易超长） | 自动生成合法短名并给出映射表，结果中映射回原名；超过 90 字符的行自动折行 |
| A7 | `mplus_engine.R::standardize_mplus_results` | 置信区间读取 `lower_2.5ci` / `upper_2.5ci`，这两列不存在（MplusAutomation 中是 `ci.unstandardized` 表的 `low2.5`/`up2.5`）；标准化估计按行号硬拼；`LL_converged` 字段不存在 | 按 `paramHeader + param` 键合并，正确读取 CI 表；收敛状态从 `.out` 文本判断 |
| A8 | `r_engine.R::standardize_lavaan_results` | 访问不存在的 slot `fit@Fit@warnings`，一调用就报错 | 运行时捕获 lavaan 警告并保存 |
| A9 | 批处理执行 (`server_main.R`) | 整个批处理在 Shiny 主线程同步循环中执行：进度条在运行过程中不会刷新，"停止"按钮无效，长任务期间整个会话卡死（线上版还会阻塞其他用户） | 每个模型放到独立后台 R 进程（callr/processx）中运行，真正并行、实时进度、可取消、可设置单模型超时 |
| A10 | 进度条 | `sprintf("%d%%", progress)` 遇到非整数进度（如 33.3）直接报错 | 重写 |
| A11 | 模型库按钮 | 在 `observe()` 中对每个模型动态 `observeEvent`，模型库每变化一次就重复注册一次。删除、重命名会触发多次，模型越多越严重 | 改为单一事件委托（JS → 一个 input） |
| A12 | 重命名 | `model_lib$models[[id]] <- model` 修改的是副本，重命名实际无效 | 模型库改为 `reactiveVal` 数据框 |
| A13 | `engine_manager.R` 等 | 闭包通过 `<<-` 修改的是构造函数里的局部副本，返回对象的 `version` 等字段始终是 `NULL` | 引擎层改为无状态函数 |
| A14 | `run_with_fallback` | Mplus 失败后会把 **Mplus 语法交给 lavaan 运行**（或反之），而且是悄悄切换引擎 | 删除；引擎由用户明确选择 |
| A15 | 下载图表 | 无论选择哪种图、哪种格式（PDF/SVG），下载的始终是拟合指标 PNG | 下载内容与当前图一致，支持 PNG/PDF/SVG |
| A16 | 结果匹配 | 运行状态靠模型名的子串 `grepl` 匹配，名字互为子串时会错配 | 统一用 `model_id` |

## B 类：统计方法

| # | 问题 | v3.0.0 处理 |
|---|---|---|
| B1 | lavaan 路径完全没有 bootstrap，间接效应只给 Delta 法（正态近似）CI；间接效应 a·b 的抽样分布通常是偏态的 | 可选 bootstrap（默认 2000 次，固定种子可复现），支持 percentile（默认）/ bias-corrected（≈Mplus `bcbootstrap`）/ BCa / normal |
| B2 | 两条 lavaan 路径的设定互相矛盾：`r_engine` 用 `ML + fixed.x=FALSE`，`batch_processor` 用 `MLR + std.lv=TRUE`，都不在界面上显示。`std.lv=TRUE` 会改变非标准化系数（和 a·b）的尺度 | 所有估计设置（估计方法、缺失处理、标识方式、bootstrap、有序变量）都在界面上显式设置，并写入结果报告 |
| B3 | 批量跑大量变量组合后只看 p < .05，没有多重比较意识（分叉路径 / p-hacking 风险） | 结果页的"焦点效应"表对整批检验做多重比较校正（Holm / BH-FDR / Bonferroni / 不校正），并报告检验族大小、显著比例，以及规格曲线图 |
| B4 | 饱和模型（df = 0）的 CFI=1、RMSEA=0 被标为 "Excellent" | df = 0 时标注"饱和模型，拟合指标无信息"，不做优劣判断 |
| B5 | 稳健估计（MLR/MLM/WLSMV）仍报告未校正的 χ²/CFI/RMSEA | 自动报告 scaled χ² 和 robust CFI/TLI/RMSEA |
| B6 | 收敛之外没有可接受性检查：负方差（Heywood case）、协方差矩阵非正定、bootstrap 失败次数 | 每个模型给出状态：OK / 警告 / 不可接受 / 失败，并列出原因 |
| B7 | 允许同一变量同时出现在两个角色中（如 X = M），这类模型没有意义 | 默认排除；可在界面关闭 |
| B8 | 全组合爆炸。成对数据（夫妻、亲子）需要"H/W 同步"，旧原型靠 `inp_select.py` 按文件名位置事后过滤 | 新增"联动组"：同组占位符按位置一一配对（zip），不做笛卡尔积 |
| B9 | 描述统计没有偏度/峰度、缺失比例，也没有样本量提示 | 增加偏度、峰度（超额）、缺失率、完整个案比例；偏度/峰度超过常用阈值（|偏度|>2，|峰度|>7）时提示考虑 MLR 或 bootstrap |
| B10 | WLSMV + FIML 不兼容但没有处理 | WLSMV/ULSMV 自动改用 pairwise，并在报告中注明 |
| B11 | 可复现性：没有记录种子、软件版本、设置 | 导出的 Excel 含"方法说明"工作表（引擎及版本、全部设置、种子、校正方法、时间）；ZIP 包含每个模型的语法和 `reproduce.R` |

## C 类：界面与交互

- 界面文字是机器翻译残留（如 "Please firstSelectdata file"、"AndrowsNumber of tasks"）。v3.0.0 全部重写，并提供**中文 / English**两种语言（设置页切换，或 URL 参数 `?lang=zh` / `?lang=en`）。
- 很多控件没有任何作用：并行任务数、检查点间隔、失败重试、暂停、主题、工具提示、动画、日志级别、内存、缓存。v3.0.0 删除无效控件，保留的控件都有实际作用。
- 帮助页宣称有"断点续跑""失败重试""导出 PDF"等功能，实际都不存在。帮助内容已按真实功能重写。
- 只有 Windows 能用"浏览文件夹"，线上版也会显示本机路径输入框。线上版改为"下载 ZIP"；本地版保留输出目录设置。
- 结果页缺少跨模型的效应汇总，也无法一键导出完整结果。新增"焦点效应"、"模型详情"（语法 / 输出原文 / 警告）以及 Excel + ZIP 导出。

## D 类：工程

- `server_main.R` 有 2714 行，按页面拆分为多个文件。
- 引擎抽象层（`engine_interface` / `engine_manager`）实际上没有被批处理调用，三处地方各自拼 Mplus 输入。v3.0.0 合并为 `R/core/` 下的纯函数，可单独测试。
- `module_loader.R` 加载的 `data/data_manager.R`、`templates/template_engine.R` 等 7 个文件不存在。
- README 引用的 `run_app.R` 不存在；根目录有误建的空文件 `git`；版本号在 README / app.R / CHANGELOG 中互相不一致。
- 测试目录在 2.1.1 中被删除。v3.0.0 新增 `tests/testthat`，用 `Rscript tests/run_tests.R` 运行。
