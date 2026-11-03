# SEM Batch Processor 3.0

一个基于 Shiny 的**批量结构方程模型**分析工具，面向心理学等社会科学研究。
写一个带 `{{占位符}}` 的模型模板，为每个占位符选择一个或多个变量，程序会估计所有组合，
然后在全部模型之间比较拟合指标，并汇总焦点效应（如间接效应），同时做多重比较校正。

*English summary at the end.*

## 两个版本

| | 线上版 | 本地版 |
|---|---|---|
| 运行位置 | shinyapps.io / Posit Connect 等服务器 | 自己的电脑（`shiny::runApp()`） |
| 引擎 | R / lavaan | R / lavaan，以及 **Mplus**（需自备授权） |
| 限制 | 模型数、bootstrap 次数、并行进程数有上限 | 上限较宽松（见 `config/default.yaml`） |
| 数据 | 只保存在会话临时目录，关闭页面后即删除 | 保存在本机 |

运行模式会自动识别：在 shinyapps.io / Posit Connect 上为线上版，否则为本地版。
也可以用环境变量 `SEMBP_MODE=online|local` 指定。

## 主要功能

- **数据**：支持 CSV/TSV（自动识别 UTF-8 / GBK 编码）、Excel、SPSS、Stata；可声明缺失值编码（如 -999）；描述统计（含偏度、峰度及非正态提示）；缺失模式分析；派生变量（中心化、交互项、题目均分）。
- **模型设计**：内置 10 个模板，lavaan 和 Mplus 两种语法各一套，包括简单 / 并行 / 链式中介、调节、有调节的中介（Hayes 模型 7）、APIM、APIMeM、CFA、潜变量中介、带协变量的回归。
  - **联动组**：成对数据（夫妻、亲子、前后测）的占位符按位置一一配对，不做交叉组合。名称以 `_H/_W`、`_1/_2` 等结尾的占位符会自动建议联动。
  - **题目组** `{{F1_items:set}}`：一次代入一整组题目。
  - 默认跳过同一变量出现在两个位置的组合。替换是严格的，绝不会悄悄删掉模型中的语句。
- **批量运行**：在后台 R 进程中并行运行，显示实时进度和预计剩余时间，可取消，可设置单模型时限。lavaan 的估计方法、缺失处理、bootstrap（百分位 / 偏差校正 / BCa，可设种子）、标识方式、有序变量都在界面上显式设置；程序做的自动调整会逐条列出。
- **Mplus**：程序自动生成 TITLE、DATA、NAMES 和 `MISSING = .`。超过 8 个字符的变量名会映射为短名，结果中再还原为原名；超过 90 个字符的行会自动折行。模板中声明的缺失编码如果仍出现在数据里，程序会给出警告。
- **结果**：
  - 拟合汇总：稳健估计报告校正 χ² 和稳健 CFI/TLI/RMSEA；饱和模型单独标注。
  - 可接受性检查：负方差、标准化值超过 ±1、矩阵非正定、bootstrap 失败次数。
  - 焦点效应：Holm / BH / Bonferroni 校正，可整批作为一个检验族，也可按效应分别校正；提供效应汇总和规格曲线图。
  - 模型详情：显示语法和引擎原始输出。
- **导出**：Excel（含方法说明：全部设置、种子、软件版本）；ZIP（全部模型语法与输出、分析数据，以及 `reproduce.R`）。
- **界面**：中文 / English，可在“设置”页或用 URL 参数 `?lang=zh` 切换。

## 安装与运行（本地版）

需要 R ≥ 4.1。

```r
install.packages(c("shiny", "shinydashboard", "DT", "lavaan", "callr", "processx",
                   "openxlsx", "yaml", "zip", "readxl", "haven", "semPlot"))
# 使用 Mplus 引擎时还需要：
install.packages("MplusAutomation")
```

在项目文件夹中运行：

```r
shiny::runApp()
```

程序会在 PATH 和常见安装目录中查找 Mplus，例如 `C:/Program Files/Mplus/Mplus.exe` 和 `/Applications/Mplus/mplus`。
如果没有找到，可以在“设置”页填写路径，或设置环境变量 `SEMBP_MPLUS_PATH`。

## 部署（线上版）

```r
rsconnect::deployApp()
```

shinyapps.io 上会自动使用线上模式。模型数、bootstrap 次数、上传大小等上限在 `config/default.yaml` 中设置，
本地覆盖配置可以写在 `config/local.yaml`（不纳入版本控制）。

## 项目结构

```
app.R                 入口：加载包与源码、读取配置、启动 Shiny
config/default.yaml   运行模式、语言、上限
R/core/               与界面无关的分析核心（可单独测试；后台进程也会加载）
  template.R          占位符解析、严格替换、组合生成、联动组、题目组
  data_io.R           数据读取、编码识别、缺失编码、描述统计、派生变量
  lavaan_runner.R     lavaan 选项处理、语法检查、拟合与结果提取
  mplus_runner.R      Mplus 变量名映射、.dat/.inp 生成、运行与输出解析
  results.R           拟合汇总、焦点效应、多重比较校正、规格曲线
  job_queue.R         后台并行作业队列（callr）
  export.R            Excel / ZIP / reproduce.R
R/app/                Shiny 界面与服务端（按页面拆分）、中英文翻译、帮助页
templates/lavaan/     lavaan 模板（.lav）
templates/mplus/      Mplus 模板（.inp）
tests/                testthat 测试：Rscript tests/run_tests.R
docs/REVIEW.md        对 2.1.x 的审查记录及处理方式
```

## 测试

```bash
Rscript tests/run_tests.R
```

## 统计方法说明

见程序内的“帮助与方法说明”页。要点：

- 间接效应建议使用 bootstrap 置信区间。
- 拟合指标的判断标准只是经验法则。
- 不可接受解不应解释。
- 批量检验必须做多重比较校正，并报告全部模型。

---

## English summary

SEM Batch Processor runs many structural equation models from one template.
Placeholders such as `{{X}}`, `{{M}}`, `{{Y}}` are filled with every combination of the selected variables.
Paired (dyadic) placeholders can be *linked* so that they are not crossed.
Item sets (`{{items:set}}`) substitute a whole scale at once.

Models run in parallel background processes with live progress and can be cancelled.
The results pages compare fit (robust indices, saturated models flagged) and run admissibility checks.
Focal effects across models are summarised with Holm / BH / Bonferroni correction and a specification-curve plot.
Results export to Excel and ZIP, including a methods sheet and a `reproduce.R` script.

The **online** version (shinyapps.io) uses lavaan only.
The **local** version can also drive **Mplus**: it writes NAMES / MISSING itself, maps variable names to Mplus' 8-character limit (and back), and wraps long lines.
The interface is available in English and Chinese.

Run locally with `shiny::runApp()`, test with `Rscript tests/run_tests.R`.

Author: Qingguang "Victor" Zheng · zhengqg@mail.bnu.edu.cn
