# 开发辅助脚本

| 文件 | 用途 |
|---|---|
| `make_mock_data.R` | 按已知总体模型生成模拟数据（400 对夫妻；含 -999 缺失、GBK 中文列名、xlsx、sav 版本）。`Rscript dev/make_mock_data.R`，输出到 `dev/mock_data/`（不纳入版本控制）。真值写在脚本开头的注释里。 |
| `e2e_smoke.js` | 用 Playwright 在浏览器里走一遍完整流程（上传数据 → 中介模板 → 12 个模型 → bootstrap → 结果与图表），并逐页截图。 |

```bash
Rscript dev/make_mock_data.R
SEMBP_LANG=zh Rscript -e 'shiny::runApp(".", port = 3838, launch.browser = FALSE)' &
mkdir -p /tmp/sembp/shots && node dev/e2e_smoke.js /tmp/sembp "$PWD"
```
