# Changelog

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
