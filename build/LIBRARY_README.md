# OneFileLinux Build Library System

## Overview

The OneFileLinux build system uses a modular library approach to ensure consistent behavior across all build environments (local, Docker, and GitHub Actions). This README explains the library structure and how to maintain it.

## Library Structure

The library files are numbered in the 80-89 range and are designed to be loaded in sequence:

| File                  | Purpose                                                      |
|-----------------------|--------------------------------------------------------------|
| 80_common.sh          | Core utilities, logging, environment detection               |
| 81_error_handling.sh  | Error handling and reporting                                 |
| 82_build_helper.sh    | Cross-environment build utilities (file operations, etc.)    |
| 83_config_helper.sh   | Configuration management and feature flags                   |
| 84_build_core.sh      | Core build functionality (kernel, ZFS, EFI file creation)    |
| 85_dependency_helper.sh | Package dependency management for all environments         |

## Loading Mechanism

The libraries use a smart loading system that:
- Prevents duplicate loading
- Handles dependencies between libraries
- Works correctly regardless of which script initiates the loading

To use the libraries in a script:

1. First source the core library directly:
```bash
source ./80_common.sh
```

2. Then use the source_libraries function to load all libraries:
```bash
source_libraries "."  # The path parameter is the directory containing the libraries
```

## Dependency Management

The `85_dependency_helper.sh` library provides consistent dependency management across all build environments:

- Maintains a single source of truth for package dependencies
- Maps generic package names to distribution-specific package names
- Generates appropriate package installation commands for each OS
- Can synchronize dependencies between Docker and local builds

### Synchronizing Dependencies

To ensure Docker and local builds use the same dependencies, use the sync-dependencies.sh script:

```bash
./tools/sync-dependencies.sh
```

Options:
- `--dry-run`: Show what would be updated without making changes
- `--verbose`: Display detailed information
- `--force`: Force update even if files are in sync

## Best Practices

1. **Adding New Dependencies**:
   - Add them to `85_dependency_helper.sh`
   - Run `./tools/sync-dependencies.sh` to update all environments

2. **Creating New Build Steps**:
   - Use the existing scripts as templates
   - Source libraries with `source ./80_common.sh` followed by `source_libraries "."`
   - Use modular functions from appropriate libraries

3. **Cross-Environment Compatibility**:
   - Use the environment detection functions in 80_common.sh
   - Use the file operation helpers in 82_build_helper.sh for cross-platform file handling
   - Handle permissions appropriately for Docker/GitHub Actions
   
4. **Adding New Features**:
   - Add feature flags to `84_build_core.sh`
   - Update configuration handling in `83_config_helper.sh`
   - Ensure the feature can be toggled consistently across all build paths

## Troubleshooting

Common issues and solutions:

1. **Library loading errors**:
   - Ensure all library files exist and are executable
   - Check the loading order in source_libraries function

2. **Permission issues in Docker**:
   - Use environment-aware functions from 82_build_helper.sh
   - Consider using elevated privileges with sudo when necessary

3. **Dependency issues**:
   - Update the dependency helper and synchronize
   - Check for missing mappings in map_package_name function