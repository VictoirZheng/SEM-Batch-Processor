# User Guide

Welcome to the SEM Batch Processor! This guide will help you get started quickly.

## Quick Start

### 1. Launch the Application

```r
# Run in R or RStudio
source("run_app.R")
```

The application will automatically open in your browser.

### 2. Basic Workflow

```
Data Management  Model Design  Batch Processing  Results Analysis  Visualization
```

## Detailed Instructions

### I. Data Management

#### 1.1 Upload Data

**Supported Formats**:
- CSV files (.csv)
- Excel files (.xlsx, .xls)
- Text files (.txt, .dat)

**Upload Methods**:
1. Click the "Browse" button to select a file
2. Or drag and drop files directly to the upload area

**Data Requirements**:
- First row must be variable names
- Variable names cannot contain spaces or special characters
- Numeric variables are used for SEM analysis

#### 1.2 Data Preview

After uploading, you can view:
- Data table (first 10 rows)
- Number of variables and observations
- Descriptive statistics (mean, standard deviation, etc.)

#### 1.3 Clear Data

Click the "Clear Data" button to delete current data and re-upload.

---

### II. Model Design

There are two modes: **Manual Design** and **Auto-Combination**

#### 2.1 Manual Design Mode

**Use Case**: Design single or few models

**Steps**:

1. **Select Template**
   - Choose a preset template from the dropdown menu
   - Or select "Custom" to write your own

2. **Edit Model**
   - Use `{{placeholders}}` to mark variables for mapping
   - Example: `{{outcome}} ~ {{predictor}}`

3. **Map Variables**
   - Select actual data variables for each placeholder
   - System automatically detects placeholders

4. **Add to Model Library**
   - Click "Add to Model Library"
   - Model will be saved in the pending list

**Example**:
```
# Simple regression model
{{Y}} ~ {{X1}} + {{X2}}
```

#### 2.2 Auto-Combination Mode

**Use Case**: Batch generate multiple model combinations

**Steps**:

1. **Load Template**
   - Select template type
   - Click "Load Template"

2. **Select Multiple Variables**
   - Select **multiple** variables for each placeholder
   - Example: Select Y1, Y2, Y3 for {{outcome}}

3. **Generate Combinations**
   - Click "Generate All Combinations"
   - System automatically generates all possible models

**Example**:
- Placeholders: {{outcome}}, {{predictor}}
- Selection: outcome = [Y1, Y2], predictor = [X1, X2]
- Generates 4 models:
  - Y1 ~ X1
  - Y1 ~ X2
  - Y2 ~ X1
  - Y2 ~ X2

#### 2.3 Model Library Management

**View Models**:
- View all pending models in "Current Model Library"
- Display model names and creation time

**Delete Models**:
- Click the delete button next to a model
- Or click "Clear All Models"

---

### III. Batch Processing

#### 3.1 Select Engine

**R/lavaan Engine** (Recommended):
- Free and open source
- Easy installation
- Suitable for standard SEM analysis

**Mplus Engine** (Optional):
- Requires commercial license
- More powerful features
- Suitable for complex models

#### 3.2 Run Analysis

1. Ensure data is uploaded
2. Ensure models are in the model library
3. Click "Run Batch Analysis"
4. Wait for processing to complete (progress bar displayed)

#### 3.3 View Progress

- Real-time processing progress display
- Show number of successful/failed models
- Display processing log

---

### IV. Results Analysis

#### 4.1 Fit Indices Comparison

**View Table**:
- Display fit indices for all models
- Includes: χ, df, CFI, TLI, RMSEA, SRMR, AIC, BIC

**Filter and Sort**:
- Click column headers to sort
- Use search box to filter
- Export as CSV or Excel

**Fit Indices Interpretation**:
- **CFI/TLI**: > 0.95 excellent, > 0.90 acceptable
- **RMSEA**: < 0.05 excellent, < 0.08 acceptable
- **SRMR**: < 0.08 good
- **AIC/BIC**: Lower is better (for model comparison)

#### 4.2 Parameter Estimates

View for each model:
- Path coefficients
- Factor loadings
- Variances and covariances
- Standard errors and p-values

#### 4.3 Model Diagnostics

- Convergence status
- Warning messages
- Modification indices

---

### V. Visualization

#### 5.1 Fit Indices Comparison Chart

**Bar Chart**:
- Compare fit indices across different models
- X-axis: Model names
- Y-axis: Fit index values

#### 5.2 Fit Indices Distribution Plot

**Histogram and Boxplot**:
- Show distribution of CFI and RMSEA
- Mark excellent/acceptable threshold lines
- Suitable for batch model analysis

#### 5.3 Chi-Square Value Comparison

**Bar Chart**:
- Compare χ values across models
- Lower values indicate better fit

#### 5.4 Path Diagram

**Individual Model Path Diagram**:
- Select model to view
- Display path relationships between variables
- Label path coefficients

**Export Charts**:
- Click "Download Chart"
- Save as PNG or PDF

---

### VI. Settings

#### 6.1 Engine Settings

**View Engine Status**:
- R/lavaan engine information
- Mplus engine information

**Switch Default Engine**:
- Select default engine to use
- Affects subsequent analyses

#### 6.2 Interface Settings

**Theme**:
- Default (flatly)
- Dark (darkly)
- Simple (cosmo)

**Language**:
- English
- SimplifiedinChinese (Simplified Chinese)

---

## Common Template Descriptions

### 1. Confirmatory Factor Analysis (CFA)

**Purpose**: Validate preset factor structure

**Template**:
```
# Factor 1
F1 =~ item1 + item2 + item3

# Factor 2
F2 =~ item4 + item5 + item6

# Factor correlation
F1 ~~ F2
```

### 2. Path Analysis

**Purpose**: Analyze causal relationships between observed variables

**Template**:
```
# Outcome predicted by predictors
Y ~ X1 + X2 + X3

# Correlations among predictors
X1 ~~ X2
X1 ~~ X3
X2 ~~ X3
```

### 3. Mediation Model

**Purpose**: Test mediation effects

**Template**:
```
# Direct effect
Y ~ X

# Mediation paths
M ~ X
Y ~ M
```

### 4. Moderation Model

**Purpose**: Test moderation effects

**Template**:
```
# Main effects
Y ~ X + M

# Interaction effect
Y ~ X:M
```

### 5. Full SEM

**Purpose**: Includes measurement model and structural model

**Template**:
```
# Measurement model
F1 =~ item1 + item2 + item3
F2 =~ item4 + item5 + item6

# Structural model
F2 ~ F1
```

---

## Usage Tips

### 1. Variable Naming Recommendations

- Use meaningful variable names
- Avoid using non-ASCII characters (may cause encoding issues)
- Use underscores for separation: `outcome_var`, `predictor_1`

### 2. Model Naming Recommendations

- Use descriptive names
- Include key variable information
- Example: `Y1_X1_mediation`, `CFA_3factors`

### 3. Batch Analysis Recommendations

- Test with a few models first
- Confirm data and models are correct before batch running
- Save results regularly

### 4. Results Interpretation Recommendations

- Prioritize fit indices
- Check reasonableness of parameter estimates
- Pay attention to warnings and error messages

### 5. Performance Optimization

- Avoid running too many models at once (recommend <100)
- Complex models may take longer
- Close unnecessary browser tabs

---

## Frequently Asked Questions

### Q1: No response after uploading data?
**A**: Check data format, ensure first row is variable names and file encoding is correct.

### Q2: Model execution failed?
**A**: Check error messages. Common causes:
- Variable names don't match
- Model syntax error
- Missing values in data

### Q3: How to save results?
**A**: Click export button on Results Analysis page, select CSV or Excel format.

### Q4: Mplus engine not available?
**A**: Ensure Mplus software and MplusAutomation package are installed.

### Q5: How to add custom templates?
**A**: Add .txt files to `templates/r/` or `templates/mplus/` folder.

### Q6: Can longitudinal data be analyzed?
**A**: Yes, use custom models to write longitudinal CFA or growth curve models.

### Q7: Is multi-group analysis supported?
**A**: R/lavaan supports it using the `group` parameter. Mplus also supports it.

### Q8: How to handle missing values?
**A**: lavaan uses FIML method by default. Mplus can specify in syntax.

---

## Keyboard Shortcuts

- **Ctrl + Upload**: Quick upload data (Windows)
- **Cmd + Upload**: Quick upload data (Mac)

---

## Getting Help

- **Help Page**: Click "Help" in the left menu
- **Documentation**: See README.md and INSTALLATION.md
- **Issue Reporting**: Submit GitHub Issue

---

**Version**: 2.1.1-EN  
**Last Updated**: 2025-11-05
