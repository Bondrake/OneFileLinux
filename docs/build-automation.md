# OneFileLinux Build Automation

This document describes the implementation of build automation for OneFileLinux.

## Automation Overview

The OneFileLinux build system has been automated to make building easier and more consistent across different environments. The automation focuses on:

1. **Dependency Management**: Automatically installing and configuring required tools
2. **Cross-Platform Support**: Working on various Linux distributions and macOS
3. **Multiple Build Methods**: Supporting both local SBCL builds and container builds (Docker/Podman)
4. **Simplified Interface**: Providing a simple command-line interface for building

## Automation Components

### 1. Bootstrap Script (`build.sh`)

The main entry point for build automation is `build.sh`, which:

- Detects the operating system/distribution
- Installs required dependencies (SBCL, Quicklisp, libraries)
- Configures the build environment
- Provides instructions for building with SBCL directly or with containers

### 2. Configuration Management (`build-config.sh`)

A separate configuration file containing:

- Distribution-specific package lists
- Container engine installation instructions
- URLs for dependency downloads
- Path configurations
- Build profile settings

### 3. Container Integration

Support for both Docker and Podman as container engines:

- Auto-detection of available container engines
- Instructions for container engine installation
- Setup of proper permissions and groups
- Common command interface that works with either engine

## Supported Build Methods

### Local SBCL Build

The direct approach using SBCL on the local system:

```bash
sbcl --load "main.lisp" -- [options]
```

### Container Build (Docker/Podman)

The isolated approach using containers:

```bash
# With Docker
docker build -t onefilelinux-builder .
docker run --rm -v $(pwd)/output:/output onefilelinux-builder [options]

# With Podman
podman build -t onefilelinux-builder .
podman run --rm -v $(pwd)/output:/output onefilelinux-builder [options]
```

## Supported Operating Systems and Distributions

### For Local SBCL Builds

The local build method is supported on Linux distributions:

- **Debian-based**: Ubuntu, Debian, Pop!_OS, Mint, Elementary, etc.
- **Red Hat-based**: Fedora, RHEL, CentOS, Rocky, Alma, etc.
- **Arch-based**: Arch Linux, Manjaro, EndeavourOS, etc.
- **SUSE-based**: openSUSE, SUSE Linux Enterprise
- **Other**: Alpine, Gentoo, Void Linux

Each distribution has specific package lists and installation commands configured.

### For Container Builds

Container-based builds are supported on any operating system that can run Docker or Podman:

- **Linux**: All major distributions
- **macOS**: Via Docker Desktop or Podman Machine
- **Windows**: Via Docker Desktop or WSL2 + Podman

> **Note**: macOS and Windows support is limited to container-based builds only. Direct SBCL builds require Linux due to kernel compilation requirements and system dependencies.

## Usage Examples

### Basic Usage

```bash
# Run the bootstrap script
./build.sh

# Follow the interactive prompts
```

### Non-Interactive Usage

```bash
# Setup for local build only
./build.sh --local

# Setup for Docker build only
./build.sh --docker

# Setup for Podman build only
./build.sh --podman

# Setup for both local and container builds
./build.sh --all
```

### Specifying Container Engine

```bash
# Use specific container engine
./build.sh --all --container-engine podman
```

## Design Principles

The build automation follows these design principles:

1. **Minimal Shell Scripting**: Shell scripts only handle dependency installation and bootstrapping, with the core build logic in Common Lisp
2. **Modularity**: Separate configuration from code for easier maintenance
3. **Detect-and-Adapt**: Automatically detect environment and adapt accordingly
4. **Graceful Fallbacks**: If preferred tools are unavailable, offer alternatives
5. **Clear Instructions**: Provide explicit instructions for next steps

## Future Enhancements

Planned enhancements to the build automation:

1. **Configuration Generator**: Tool to generate custom build configurations
2. **Build Verification**: Automated testing of built EFI files
3. **Continuous Integration**: Enhanced GitHub Actions workflow
4. **Dependency Caching**: Improved caching for faster repeat builds
5. **Additional Platforms**: Support for more exotic platforms