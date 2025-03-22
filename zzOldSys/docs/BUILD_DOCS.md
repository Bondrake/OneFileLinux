# OneFileLinux Build Documentation

## Quick References

For detailed information about the build system and available options, please refer to:

- [Build System README](../build/README.md) - Core build process documentation
- [Docker Build System README](../docker/README.md) - Containerized build instructions
- [User Guide](USER_GUIDE.md#detailed-build-script-options) - Detailed build script options

## Build System Architecture

The OneFileLinux build system employs a structured approach with modular components and library scripts to ensure reliable builds across different environments.

### Script Organization

#### Library Scripts (80-89 range)

The build system has been reorganized to use a numbered library script system in the 80-89 range:

- **80_common.sh**: Basic utilities, logging functions, and environment detection
  - Provides color definitions, logging, banners, and environment checks
  - Has minimal dependencies and loads quickly
  - Used by all other scripts for consistent output formatting

- **81_error_handling.sh**: Error management and recovery
  - Handles error trapping, reporting, and recovery
  - Performs prerequisite checking for build scripts
  - Tracks build progress for resuming interrupted builds

- **82_build_helper.sh**: Environment-aware build utilities
  - Provides file and directory operations with proper permissions
  - Implements extraction with fallback methods for different environments
  - Contains system configuration and build helper functions

#### Build Scripts (00-10, 99 range)

The main build process is divided into numbered steps:

- **00_prepare.sh**: Environment preparation and dependency installation
- **01_get.sh**: Component downloading and extraction
- **02_chrootandinstall.sh**: Alpine Linux configuration and package installation
- **03_conf.sh**: System services and settings configuration
- **04_build.sh**: Kernel building and EFI file creation
- **99_cleanup.sh**: Build artifact cleanup

#### Entry Points

- **build.sh**: Main entry point for the build process
- **docker/build-onefilelinux.sh**: Docker-based build launcher

### Using the Build System

When creating new scripts or modifying existing ones, follow this sourcing order:

1. Set script name: `SCRIPT_NAME=$(basename "$0")`
2. Source common utilities: `source ./80_common.sh`
3. Source error handling: `source ./81_error_handling.sh`
4. Initialize error handling: `init_error_handling`
5. Source build helper if needed: `source ./82_build_helper.sh`

This ensures proper initialization and consistent behavior across the build system.

### Environment Detection

The build system automatically adapts to different environments:

- **Standard Environment**: Uses the local system directly
- **Docker Container**: Adapts to container constraints and permissions
- **CI/CD Systems**: Handles restricted permissions in GitHub Actions

Environment detection is centralized in the common library to avoid inconsistencies.

### Key Improvements

Recent improvements to the build system include:

- **Better Separation of Concerns**: Clear responsibilities for each script
- **Reduced Duplication**: Shared functionality moved to library scripts
- **Standardized Banner Display**: Consistent headers across all scripts
- **Extraction Caching**: Marker files to avoid redundant extractions
- **Verbose Output Control**: Support for detailed build output (`--make-verbose`)

For more detailed build options and customization, refer to the [User Guide](USER_GUIDE.md#detailed-build-script-options).