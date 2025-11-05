# Installation Guide

## System Requirements

### Operating Systems
- Windows 10/11 (recommended)
- macOS 10.14+
- Linux (Ubuntu 18.04+)

### Software Requirements
- R >= 4.0.0
- RStudio >= 1.4 (recommended but not required)

## Installation Steps

### 1. Install R

#### Windows
1. Visit https://cran.r-project.org/bin/windows/base/
2. Download the latest R installer
3. Run the installer and follow default options

#### macOS
1. Visit https://cran.r-project.org/bin/macosx/
2. Download the R package for your system version
3. Open the .pkg file to install

#### Linux (Ubuntu/Debian)
```bash
sudo apt update
sudo apt install r-base r-base-dev
```

### 2. Install RStudio (Recommended)

1. Visit https://posit.co/download/rstudio-desktop/
2. Download RStudio Desktop for your system
3. Install RStudio

### 3. Install R Package Dependencies

Open R or RStudio and run the following commands:

```r
# Install core packages
install.packages(c(
  "shiny",
  "shinydashboard",
  "DT",
  "lavaan",
  "yaml",
  "dplyr",
  "tidyr",
  "ggplot2",
  "semPlot"
))
```

Wait for all packages to install (may take a few minutes).

### 4. Download the Project

#### Method 1: Git Clone (Recommended)
```bash
git clone https://github.com/VictoirZheng/BatchSEM_English.git
cd BatchSEM_English
```

#### Method 2: Direct Download
1. Download the project ZIP file
2. Extract to a local folder

### 5. Run the Application

In R or RStudio:

```r
# Set working directory
setwd("path/to/BatchSEM_English")

# Run the application
source("run_app.R")
```

Or:

```r
shiny::runApp("app.R")
```

The application will automatically open in your browser.

## Optional: Install Mplus Engine

If you need to use the Mplus engine:

### 1. Install Mplus Software

1. Visit https://www.statmodel.com/
2. Purchase Mplus license
3. Download and install Mplus software

### 2. Install MplusAutomation Package

```r
install.packages("MplusAutomation")
```

### 3. Configure Mplus Path

Ensure the Mplus executable is in your system PATH, or specify the path in application settings.

## Verify Installation

Run the following code to verify installation:

```r
# Check if packages are correctly installed
required_packages <- c("shiny", "shinydashboard", "DT", "lavaan", "yaml", "dplyr", "tidyr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat(sprintf(" %s not installed\n", pkg))
  } else {
    cat(sprintf(" %s installed\n", pkg))
  }
}

# Check lavaan version
cat(sprintf("\nlavaan version: %s\n", packageVersion("lavaan")))

# Check Mplus (optional)
if (require("MplusAutomation", quietly = TRUE)) {
  cat(" MplusAutomation installed\n")
} else {
  cat("ℹ MplusAutomation not installed (optional)\n")
}
```

## Common Installation Issues

### Issue 1: Package Installation Failure

**Solution**:
```r
# Change CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Reinstall
install.packages("package_name")
```

### Issue 2: Missing Dependencies

**Solution**:
```r
# Install all dependencies
install.packages("package_name", dependencies = TRUE)
```

### Issue 3: Permission Issues (Linux/macOS)

**Solution**:
```bash
# Install system-level dependencies with sudo
sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev
```

### Issue 4: Mplus Not Found

**Solution**:
- Windows: Add Mplus installation directory to system PATH
- macOS/Linux: Create symbolic link to /usr/local/bin

### Issue 5: Character Encoding Issues

**Solution**:
```r
# Set encoding
Sys.setlocale("LC_ALL", "en_US.UTF-8")
```

## Update the Application

### Method 1: Git Update
```bash
cd BatchSEM_English
git pull origin main
```

### Method 2: Manual Update
1. Download the latest version
2. Backup config/user_preferences.yaml
3. Replace all files
4. Restore user_preferences.yaml

## Uninstall

1. Delete the project folder
2. (Optional) Uninstall R packages:
```r
remove.packages(c("shiny", "shinydashboard", "DT", "lavaan"))
```

## Technical Support

If you encounter installation issues:
1. Check error messages
2. Verify R and package versions
3. Consult FAQ
4. Submit an Issue on GitHub

---

**Last Updated**: 2025-11-05
