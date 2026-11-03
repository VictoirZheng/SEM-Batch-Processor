# Changelog

## [3.0.0] - 2026-11-03

Complete rework of the analysis core and the interface. See `docs/REVIEW.md`
for the full list of problems found in 2.1.x and how each one was handled.

### Fixed (results could be wrong or the app did not work)
- Source files started with a UTF-8 byte-order mark, so the app failed to start on Linux (including shinyapps.io).
- Lines that still contained an unresolved `{{placeholder}}` (e.g. `{{outcome:Y}}`) were silently deleted, so a different model was estimated. Substitution is now strict.
- Mplus: missing values were written as `.` while templates declared `MISSING ARE ALL (-999)`; the single-step Mplus batch wrote no `NAMES`; confidence intervals were read from non-existent columns; no handling of the 8-character name and 90-character line limits.
- lavaan: `fit@Fit@warnings` does not exist; two code paths used different, hidden settings (`ML + fixed.x = FALSE` vs `MLR + std.lv = TRUE`).
- Batches ran synchronously in the Shiny session: no progress, no cancel, frozen session.
- Model-library buttons registered duplicate observers; renaming did not work; results were matched to models by substring.
- Engine "fallback" ran Mplus syntax with lavaan (and vice versa).
- Plot download always produced the fit-index PNG regardless of the selection.

### Added
- Background execution in separate R processes: parallel workers, live progress, ETA, per-model time limit, cancel.
- Link groups for paired placeholders (dyadic data), automatic suggestion for `_H/_W`, `_1/_2`, ... suffixes; item-set placeholders `{{items:set}}`; duplicate-variable combinations skipped.
- lavaan options on the Run page: estimator, missing data (FIML / FIML incl. exogenous / listwise), bootstrap with percentile / bias-corrected / BCa CIs and a seed, identification, ordered variables, confidence level; automatic adjustments are reported.
- Admissibility checks (non-convergence, negative variances, |std| > 1, non-positive-definite matrices, failed bootstrap draws); robust fit indices for robust estimators; saturated models flagged.
- Focal-effect table across models with Holm / BH / Bonferroni correction (batch-wide or per effect), effect summary, specification-curve plot.
- Data screening: encoding detection (UTF-8 / GBK), missing-value codes, skewness/kurtosis, missing patterns, derived variables (centring, product terms, mean scores).
- Export: Excel workbook with methods sheet, ZIP with all syntax/output, data and `reproduce.R`.
- Online/local modes (auto-detected) with limits for shared servers; Chinese and English interface.
- 10 consistent templates for both engines (simple/parallel/serial mediation, moderation, moderated mediation, APIM, APIMeM, CFA, latent mediation, regression with covariates).
- Test suite (`Rscript tests/run_tests.R`).

### Removed
- Non-functional controls (checkpoint interval, retry, pause, theme, tooltips, animations, log level, memory, cache) and unused dependencies (shinyWidgets, plotly, dplyr, tidyr, stringr, psych, progress, crayon, shinyjs).
- Outdated documents (USER_GUIDE, INSTALLATION, PROJECT_SUMMARY, PROJECT_STRUCTURE, TRANSLATION_SUMMARY); their content is now in README.md and the in-app help.

## [2.1.2] - 2025-11-04

### Fixed
- Fixed "zero-length parameter" error in R engine batch analysis
- Improved error handling for lavaan convergence checks
- Enhanced fault tolerance for parameter estimate extraction
- Fixed path selection logic for Mplus batch processing browse button
- Improved user feedback for folder selection

### Improved
- Optimized auto-combination mode button layout, reduced button spacing
- Used flex layout to make buttons more compact
- Improved detail level of error messages

## [2.1.1] - 2025-11-04

### Maintenance
- Streamlined project structure, removed redundant files and folders
- Deleted APIMeM old project folder
- Deleted tests folder
- Cleared temporary output folders (mplus_output, logs)
- Deleted old spec folder
- Updated .gitignore, added more ignore rules
- Created complete project documentation (README.md, INSTALLATION.md, PROJECT_STRUCTURE.md)

## [2.1.0] - 2025-11-04

### Improved
- Fixed engine status display synchronization issue - Home page now correctly displays currently selected engine
- Optimized data preview table display - Disabled auto-wrap, maintains consistent row height
- Improved button display logic - Hides irrelevant validation buttons under Mplus engine
- Fixed Windows file browsing functionality - Added platform detection and friendly prompts
- Adjusted settings page engine order - R engine configuration displayed first
- Corrected visualization labels - Changed "Parameter Estimate Distribution" to "Fit Indices Distribution"
- Removed "Model Success Rate" visualization option - Simplified interface
- Optimized auto-combination mode button layout - Removed redundant buttons
- Improved status synchronization after Mplus batch run - Pending model list correctly displays run results

### Added
- Added 4 R/lavaan templates: CFA, Path Analysis, Moderation Model, Full SEM
- Added 4 Mplus templates: CFA, Path Analysis, Moderation Model, Full SEM
- Added clear template button (auto-combination mode)
- Created .gitignore file for version control management

### Maintenance
- Cleaned up project redundant files (-v.out, test data, prototype code, etc.)
- Optimized project directory structure
- Reorganized template folders (templates/r/ and templates/mplus/)

## [2.0.0] - 2025-XX-XX

### Initial Release
- Dual engine support (R/lavaan and Mplus)
- Data management functionality
- Model design (manual and auto-combination modes)
- Batch processing
- Results analysis and visualization

## [2.1.1-EN] - 2025-11-05

### English Version Release
- Complete English translation of all UI elements
- Translated all documentation files
- Translated all code comments and messages
- Translated configuration files and templates
- Maintained full functional parity with Chinese version
- Updated project metadata for English version
