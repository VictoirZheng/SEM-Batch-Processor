# Project Structure Documentation

This document provides a detailed explanation of the SEM Batch Processor project structure and the purpose of each file.

## Directory Structure

```
BatchSEM_English/

  Core Files
    app.R                    # Shiny application entry point
    run_app.R               # Application launch script
    README.md               # Project documentation
    INSTALLATION.md         # Installation guide
    CHANGELOG.md            # Version changelog
    PROJECT_STRUCTURE.md    # This file
    .gitignore              # Git ignore configuration

  R/ - R Code Directory
   
     config/ - Configuration Module
       app_config.R        # Global application configuration
   
     engines/ - Engine Implementations
       engine_interface.R  # Engine interface definition
       engine_manager.R    # Engine manager
       r_engine.R          # R/lavaan engine implementation
       mplus_engine.R      # Mplus engine implementation
   
     ui/ - User Interface
       ui_main.R           # Main UI definition
   
     server/ - Server Logic
       server_main.R       # Main server logic
   
     utils/ - Utility Functions
       batch_processor.R   # Batch processor
       error_handler.R     # Error handling
       helpers.R           # Helper functions
       logger.R            # Logging system
       model_library.R     # Model library management
       mplus_batch_processor.R  # Mplus batch processing
       path_diagram.R      # Path diagram rendering
       variable_mapper.R   # Variable mapping
       workflow_manager.R  # Workflow management
   
     modules/ - Module System
        module_loader.R     # Module loader

  templates/ - Model Templates
   
     r/ - R/lavaan Templates
       cfa.txt             # Confirmatory Factor Analysis
       path_analysis.txt   # Path Analysis
       mediation.txt       # Mediation Model
       moderation.txt      # Moderation Model
       sem_full.txt        # Full SEM
       basic_sem.txt       # Basic SEM
   
     mplus/ - Mplus Templates
        cfa_mplus.txt
        path_analysis_mplus.txt
        mediation_mplus.txt
        moderation_mplus.txt
        sem_full_mplus.txt
        basic_sem_mplus.txt
        apim_mplus.txt

  config/ - Configuration Files
    default.yaml            # Default configuration
    user_preferences.yaml   # User preferences

  www/ - Static Assets
    custom.js               # Custom JavaScript

  data/ - Data Directory
    (User uploaded data files)

  output/ - Output Directory
    (Analysis results output)

  mplus_output/ - Mplus Output
    (Mplus generated inp and out files)

  logs/ - Log Directory
     (Application runtime logs)
```

## Core Files Description

### app.R
Main application entry point, responsible for:
- Loading required R packages
- Initializing application configuration
- Creating Shiny application instance
- Starting application server

### run_app.R
Convenient launch script, providing:
- Automatic dependency checking
- Working directory setup
- Application startup
- Error handling

## R Code Modules

### config/ - Configuration Module
**app_config.R**: Defines global application configuration, including:
- UI theme settings
- File upload limits
- Logging level
- Default engine settings

### engines/ - Engine System
The engine system uses object-oriented design, supporting multiple SEM analysis engines:

- **engine_interface.R**: Defines standard engine interface
- **engine_manager.R**: Manages multiple engines, handles engine switching
- **r_engine.R**: R/lavaan engine implementation
- **mplus_engine.R**: Mplus engine implementation

### ui/ - User Interface
**ui_main.R**: Defines complete user interface, including:
- Page layout (using shinydashboard)
- UI components for each functional module
- Reactive UI elements
- Styles and themes

### server/ - Server Logic
**server_main.R**: Implements all server-side logic:
- Data processing
- Model execution
- Results analysis
- User interaction response
- State management

### utils/ - Utility Function Library

| File | Function |
|------|----------|
| batch_processor.R | R engine batch processing logic |
| mplus_batch_processor.R | Mplus engine batch processing logic |
| model_library.R | Model library CRUD operations |
| variable_mapper.R | Variable placeholder mapping |
| workflow_manager.R | Workflow state management |
| path_diagram.R | Path diagram rendering |
| error_handler.R | Unified error handling |
| logger.R | Logging system |
| helpers.R | General helper functions |

### modules/ - Module System
**module_loader.R**: Dynamically loads and manages application modules

## Template System

### templates/r/
Model templates for R/lavaan engine, using lavaan syntax:
- Use `{{placeholders}}` to mark variables for mapping
- Support factor analysis, path analysis, SEM, and other models
- Include explanatory comments

### templates/mplus/
Model templates for Mplus engine, using Mplus syntax:
- Follow Mplus INPUT file format
- Include TITLE, DATA, VARIABLE, MODEL sections
- Support advanced Mplus features

## Configuration Files

### config/default.yaml
Default configuration file, containing:
```yaml
ui:
  theme: flatly
  max_file_size_mb: 100

engine:
  default: r
  
logging:
  level: INFO
  file: logs/app.log
```

### config/user_preferences.yaml
User personalized settings, overrides default configuration

## Data Directories

### data/
Stores user uploaded data files:
- Supports CSV, Excel, and other formats
- Automatic temporary file cleanup

### output/
Stores analysis results:
- Fit indices tables
- Parameter estimates
- Exported reports

### mplus_output/
Mplus-specific output directory:
- .inp files (input files)
- .out files (output files)
- .dat files (data files)

### logs/
Application log files:
- Split by date
- Records errors, warnings, information
- Facilitates troubleshooting

## Static Assets

### www/
Stores frontend static assets:
- **custom.js**: Custom JavaScript code
  - Drag-and-drop upload functionality
  - UI interaction enhancements
  - Animation effects

## File Naming Conventions

### R Files
- Use lowercase letters and underscores
- Module naming: `module_function.R`
- Examples: `batch_processor.R`, `model_library.R`

### Template Files
- Use lowercase letters and underscores
- R templates: `model_type.txt`
- Mplus templates: `model_type_mplus.txt`

### Configuration Files
- Use lowercase letters and underscores
- YAML format: `.yaml`
- Examples: `default.yaml`, `user_preferences.yaml`

## Code Organization Principles

### 1. Modularity
- Each file focuses on a single function
- Clear module boundaries
- Minimize inter-module dependencies

### 2. Extensibility
- Engine system supports adding new engines
- Template system supports custom templates
- Configuration system supports flexible configuration

### 3. Maintainability
- Clear code comments
- Semantic function naming
- Unified error handling

### 4. Performance Optimization
- Reactive programming avoids redundant computation
- Batch processing improves efficiency
- Asynchronous log writing

## Development Guide

### Adding New Features
1. Create new file in corresponding module directory
2. Implement feature functions
3. Add UI elements in UI
4. Add logic in Server
5. Update documentation

### Adding New Engine
1. Create new engine file in `R/engines/`
2. Implement interface defined in engine_interface
3. Register in engine_manager
4. Add corresponding templates

### Adding New Template
1. Create .txt file in `templates/r/` or `templates/mplus/`
2. Use `{{placeholders}}` to mark variables
3. Add explanatory comments
4. Add option in UI template selector

## Dependency Relationships

```
app.R
   R/config/app_config.R
   R/ui/ui_main.R
   R/server/server_main.R
      R/engines/engine_manager.R
         R/engines/r_engine.R
         R/engines/mplus_engine.R
      R/utils/batch_processor.R
      R/utils/model_library.R
      R/utils/variable_mapper.R
      R/utils/workflow_manager.R
      R/utils/logger.R
      R/utils/error_handler.R
   R/modules/module_loader.R
```

## Version Control

### .gitignore
Ignores the following files:
- R temporary files (.Rhistory, .RData)
- Log files (*.log)
- Output files (output/*, mplus_output/*)
- User data (data/*)
- Temporary files (*.tmp, *.bak)

---

**Maintainer**: Qingguang "Victor" Zheng
**Last Updated**: 2025-11-05
