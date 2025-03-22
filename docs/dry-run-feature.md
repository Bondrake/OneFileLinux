# OneFileLinux Dry Run Feature

This document explains how to use the dry run feature in the OneFileLinux build system.

## Overview

The dry run feature allows you to simulate the build process without executing all the resource-intensive steps. This is especially useful for:

- Testing configuration changes
- Debugging build issues
- Development of new build features
- Educational purposes to understand the build process
- Validating system setup without a full build

## Basic Usage

### Command Line Options

The dry run feature includes two main options:

1. `--dry-run`: Execute a full simulation of the build process
2. `--dry-run-until=STEP`: Execute the build process up to and including the specified step

### Available Steps

You can use the following steps with the `--dry-run-until` option:

- `prepare`: Environment preparation and dependency checking
- `get`: Download and extract required sources
- `chroot`: Setup chroot environment and package installation
- `conf`: System configuration 
- `build`: Kernel building and EFI creation

### Listing Available Steps

To see all available build steps:

```bash
./build.sh --list-steps
```

## Examples

### Basic Dry Run

To perform a complete dry run without actually building anything:

```bash
./build.sh --dry-run
```

This will simulate all steps of the build process in sequence, skipping resource-intensive operations.

### Running Until a Specific Step

To run the build process up to and including the preparation step:

```bash
./build.sh --dry-run-until=prepare
```

To run until the sources are downloaded and extracted:

```bash
./build.sh --dry-run-until=get
```

To run until the chroot environment is set up:

```bash
./build.sh --dry-run-until=chroot
```

### Combining with Other Options

Dry run can be combined with other build options:

```bash
# Dry run with verbose output
./build.sh --dry-run --verbose

# Dry run with a specific profile
./build.sh --dry-run --profile=minimal

# Dry run until a specific step with a custom profile
./build.sh --dry-run-until=conf --profile=standard
```

## Implementation Details

The dry run feature is implemented using the Command pattern, which allows each build step to be executed independently with specific behavior for dry run mode. During dry run:

1. Each step's `prepare` phase is executed normally
2. The `execute` phase is modified to skip resource-intensive operations
3. The `cleanup` phase is executed normally
4. Steps beyond the specified target step in `--dry-run-until` are skipped entirely

## Use Cases

### Development

When developing new build features, use dry run to test your changes without waiting for a full build:

```bash
# Make changes to prepare step
./build.sh --dry-run-until=prepare

# Make changes to configuration step
./build.sh --dry-run-until=conf
```

### Debugging

If you encounter an issue with a specific build step, use dry run to isolate and troubleshoot it:

```bash
# Debug issues in the chroot step
./build.sh --dry-run-until=chroot --verbose
```

### Testing System Configuration

Before performing a full build, validate your system setup:

```bash
./build.sh --dry-run-until=prepare
```

### Understanding the Build Process

The dry run feature is an excellent way to learn how the build system works without consuming significant system resources:

```bash
./build.sh --dry-run --verbose
```

## Limitations

- Some build steps may still execute certain commands even in dry run mode
- The dry run is a simulation and may not catch all potential issues in a real build
- Steps executed in dry run mode may show different timing than in a full build

## See Also

- [Build Instructions](build-instructions.md) - Complete build instructions
- [Build Automation](build-automation.md) - Information about build automation
- [Implementation Progress](implementation-progress.md) - Implementation status