# SEM Batch Processor v2.1.1-EN

A Shiny-based Structural Equation Modeling (SEM) batch processing tool with dual-engine support for R/lavaan and Mplus.

## Features

### Core Functionality
- **Dual Engine Support**: R/lavaan (recommended) and Mplus (alternative)
- **Data Management**: Support for CSV, Excel, and other data formats
- **Model Design**: 
  - Manual Design Mode: Freely write model syntax
  - Auto-Combination Mode: Batch generate model combinations
- **Batch Processing**: Run multiple models at once with automatic result comparison
- **Results Analysis**: Fit indices comparison, parameter estimates, model diagnostics
- **Visualization**: Fit indices distribution, chi-square comparison, path diagrams

### Template Library
Provides multiple commonly used SEM model templates:
- Confirmatory Factor Analysis (CFA)
- Path Analysis
- Mediation Models
- Moderation Models
- Full Structural Equation Models

## System Requirements

### Required
- R >= 4.0.0
- RStudio (recommended)

### R Package Dependencies
```r
# Core packages
install.packages(c(
  "shiny",
  "shinydashboard",
  "DT",
  "lavaan",
  "yaml",
  "dplyr",
  "tidyr"
))

# Optional packages (for Mplus engine)
install.packages("MplusAutomation")
```

### Mplus Engine (Optional)
To use the Mplus engine, you need:
- Mplus software (commercial license required)
- MplusAutomation R package

## Installation and Running

### Quick Start
```r
# 1. Clone or download the project
# 2. Open R or RStudio
# 3. Set working directory to project folder
setwd("path/to/BatchSEM_English")

# 4. Run the application
source("run_app.R")
```

### Or Run Directly
```r
shiny::runApp("app.R")
```

The application will automatically open in your browser at: http://127.0.0.1:XXXX

## Project Structure

```
BatchSEM_English/
 app.R                    # Application entry point
 run_app.R               # Launch script
 README.md               # Project documentation
 INSTALLATION.md         # Installation guide
 CHANGELOG.md            # Version history
 .gitignore              # Git ignore file

 R/                      # R source code
    config/            # Configuration files
       app_config.R   # Application configuration
    engines/           # Engine implementations
       engine_interface.R
       engine_manager.R
       r_engine.R
       mplus_engine.R
    ui/                # User interface
       ui_main.R
    server/            # Server logic
       server_main.R
    utils/             # Utility functions
       batch_processor.R
       error_handler.R
       helpers.R
       logger.R
       model_library.R
       mplus_batch_processor.R
       path_diagram.R
       variable_mapper.R
       workflow_manager.R
    modules/           # Module loader
        module_loader.R

 templates/             # Model templates
    r/                # R/lavaan templates
       cfa.txt
       path_analysis.txt
       mediation.txt
       moderation.txt
       sem_full.txt
    mplus/            # Mplus templates
        cfa_mplus.txt
        path_analysis_mplus.txt
        mediation_mplus.txt
        moderation_mplus.txt
        sem_full_mplus.txt

 config/               # Configuration files
    default.yaml     # Default configuration
    user_preferences.yaml  # User preferences

 www/                 # Static web assets
    custom.js       # Custom JavaScript

 data/               # Data folder (user data)
 output/             # Output folder
 mplus_output/       # Mplus output folder
 logs/               # Log folder
```

## Usage Workflow

### 1. Data Management
- Upload CSV or Excel data files
- View data preview and descriptive statistics
- Check variable types and missing values

### 2. Model Design

#### Manual Design Mode
1. Select a template or write custom model syntax
2. Use `{{placeholders}}` to mark variables for mapping
3. Map placeholders to actual data variables
4. Add to model library

#### Auto-Combination Mode
1. Load a template
2. Select multiple variables for each placeholder
3. System automatically generates all possible combinations
4. Batch add to model library

### 3. Batch Processing
- Select engine (R/lavaan or Mplus)
- Run all models in the model library
- View processing progress in real-time

### 4. Results Analysis
- View fit indices comparison table
- Filter and sort models
- Export results

### 5. Visualization
- Fit indices distribution plots
- Fit indices comparison charts
- Chi-square value comparison
- Path diagrams (individual models)

## Engine Comparison

| Feature | R/lavaan | Mplus |
|---------|----------|-------|
| Cost | Free & Open Source | Commercial License |
| Installation | R Package | Standalone Software |
| Model Syntax | Concise & Intuitive | Powerful |
| Use Cases | Standard SEM Analysis | Complex Models, Categorical Variables |
| Recommendation |  |  |

## Frequently Asked Questions

### Q: How to switch engines?
A: Switch in the engine selector at the top of the page, or change the default engine in the Settings page.

### Q: Mplus engine not available?
A: Ensure Mplus software and MplusAutomation package are installed.

### Q: How to add custom templates?
A: Add .txt files to the `templates/r/` or `templates/mplus/` folder.

### Q: Data format requirements?
A: Supports CSV, Excel, and other formats. First row should be variable names, subsequent rows are data.

### Q: How to export results?
A: Use the export button on the Results Analysis page. Supports CSV and Excel formats.

## Technical Support

- Issue Reporting: 
- Documentation: See INSTALLATION.md and USER_GUIDE.md
- Changelog: See CHANGELOG.md

## License

This project is licensed under the MIT License.

## Acknowledgments

- lavaan package: Yves Rosseel
- MplusAutomation package: Michael Hallquist
- Shiny framework: RStudio Team

---

**Version**: 2.1.1-EN  
**Last Updated**: 2025-11-05  
**Author**: Qingguang "Victor" Zheng
