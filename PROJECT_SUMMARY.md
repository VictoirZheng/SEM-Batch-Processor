# Project Summary

## Project Information

**Project Name**: SEM Batch Processor  
**Version**: 2.1.1-EN  
**Development Language**: R + Shiny  
**Last Updated**: 2025-11-05

## Project Overview

The SEM Batch Processor is a Shiny-based web application for batch running and comparing Structural Equation Models (SEM). It supports dual engines (R/lavaan and Mplus), providing an intuitive user interface and powerful batch processing capabilities.

## Core Features

### 1. Dual Engine Support
- **R/lavaan**: Free and open source, fully functional
- **Mplus**: Commercial software, powerful features
- One-click switching, seamless integration

### 2. Intelligent Model Design
- **Manual Design**: Flexibly write individual models
- **Auto-Combination**: Batch generate model combinations
- **Template System**: 8 common SEM templates

### 3. Batch Processing
- Run multiple models at once
- Real-time progress display
- Automatic result aggregation

### 4. Results Analysis
- Fit indices comparison table
- Parameter estimates viewing
- Model diagnostic information
- Results export (CSV/Excel)

### 5. Visualization
- Fit indices distribution plots
- Fit indices comparison charts
- Chi-square value comparison
- Path diagram rendering

## Technical Architecture

### Frontend
- **Framework**: Shiny + shinydashboard
- **Tables**: DT (DataTables)
- **Charts**: ggplot2 + base R graphics
- **Interaction**: JavaScript (custom.js)

### Backend
- **Engine Management**: Object-oriented design
- **Batch Processing**: Parallel processing support
- **Data Processing**: dplyr + tidyr
- **Logging System**: Custom logger

### Model Engines
- **R/lavaan**: Direct lavaan package calls
- **Mplus**: Via MplusAutomation package

## Project Structure

```
Core Code: 2,000+ lines
 UI Code: ~800 lines (R/ui/ui_main.R)
 Server Code: ~2,500 lines (R/server/server_main.R)
 Utility Functions: ~1,000 lines (R/utils/*.R)
 Engine Implementation: ~500 lines (R/engines/*.R)

Template Files: 14
 R Templates: 6
 Mplus Templates: 8

Documentation: 6 files
 README.md
 INSTALLATION.md
 USER_GUIDE.md
 PROJECT_STRUCTURE.md
 CHANGELOG.md
 PROJECT_SUMMARY.md
```

## Key Features

### 1. User-Friendly
- Intuitive wizard-style interface
- Clear workflow
- Rich tooltips and hints
- English interface

### 2. Powerful Functionality
- Support for complex SEM models
- Batch processing capability
- Multiple visualization options
- Flexible template system

### 3. Extensibility
- Modular design
- Plugin-style engine system
- Customizable templates
- Configuration file support

### 4. Stable and Reliable
- Comprehensive error handling
- Logging system
- State management mechanism
- Data validation

## Usage Statistics

### Supported Model Types
- Confirmatory Factor Analysis (CFA)
- Path Analysis
- Mediation Models
- Moderation Models
- Full Structural Equation Models
- Multi-group Analysis
- Longitudinal Models

### Supported Data Formats
- CSV
- Excel (.xlsx, .xls)
- Text files (.txt, .dat)

### Supported Export Formats
- CSV
- Excel
- PNG (charts)
- PDF (charts)

## Performance Metrics

### Processing Capacity
- Single batch: Recommended < 100 models
- Data scale: Supports 10,000+ observations
- Variable count: Supports 100+ variables

### Response Time
- Data upload: < 2 seconds
- Model validation: < 1 second
- Single model execution: 1-10 seconds (depends on complexity)
- Batch processing: Depends on number and complexity of models

## Package Dependencies

### Required Packages
```r
shiny (>= 1.7.0)
shinydashboard (>= 0.7.0)
DT (>= 0.20)
lavaan (>= 0.6-12)
yaml (>= 2.3.0)
dplyr (>= 1.0.0)
tidyr (>= 1.2.0)
```

### Optional Packages
```r
MplusAutomation (>= 1.1.0)  # Mplus engine
ggplot2 (>= 3.4.0)          # Advanced plotting
semPlot (>= 1.1.6)          # Path diagrams
```

## Development History

### v1.0 (Prototype)
- Basic Mplus batch processing
- Simple file management

### v2.0 (Refactoring)
- Introduced dual-engine architecture
- Complete UI/UX design
- Modular code structure

### v2.1 (Optimization)
- UI/UX improvements
- New templates added
- Documentation refinement
- Project streamlining

### v2.1.1-EN (English Version)
- Complete English translation
- All UI elements in English
- English documentation
- Maintained functional parity

## Future Plans

### Short-term (v2.2)
- [ ] Multi-language support enhancement
- [ ] Real-time theme switching
- [ ] Additional templates

### Mid-term (v2.5)
- [ ] Results report generation
- [ ] Model comparison tools
- [ ] Advanced visualization

### Long-term (v3.0)
- [ ] Cloud deployment support
- [ ] Collaboration features
- [ ] API interface

## Contributors

- **Lead Developer**: Qingguang "Victor" Zheng

## License

MIT License

## Acknowledgments

Thanks to the following open source projects:
- lavaan: Yves Rosseel
- MplusAutomation: Michael Hallquist
- Shiny: RStudio Team
- All R package developers

## Contact

- **GitHub**: https://github.com/VictoirZheng
- **Email**: zhengqg@mail.bnu.edu.cn

---

**Last Updated**: 2025-11-05  
**Document Version**: 1.0-EN
