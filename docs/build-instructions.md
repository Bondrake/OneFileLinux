# OneFileLinux Build Instructions

This document provides instructions for building OneFileLinux using the refactored implementation.

## Build Options

OneFileLinux can be built in multiple ways:

1. **Local SBCL Build**: Direct build using SBCL on your local system
2. **Container Build**: Isolated build using Docker or Podman 

## Prerequisites

### Common Requirements

- Git (to clone the repository)

That's it! The `build.sh` script will automatically detect and install all other dependencies based on your system, including:
- SBCL (Steel Bank Common Lisp)
- Quicklisp (Common Lisp package manager)
- Required libraries (uiop, cl-ppcre, alexandria, etc.)
- Build tools (gcc, make, etc.)
- Docker/Podman (if needed and not already installed)

### Local Build Environment

- Linux operating system (direct builds require Linux)
- Root/sudo access (for installing dependencies)

### Container Build Environment

- Any operating system that supports Docker/Podman (Linux, macOS, Windows)
- Docker Engine OR Podman
- No other dependencies needed (all are contained in the container image)

> **Important Note**: macOS and Windows users can only use the container-based build method. Direct SBCL builds are only supported on Linux due to kernel building requirements and system-specific dependencies.

## Quick Start

The easiest way to build OneFileLinux is to use our automated build script which handles all dependencies:

```bash
# Clone the repository if you haven't already
git clone https://github.com/onefilelinux/onefilelinux.git
cd onefilelinux

# Run the build setup script
./build.sh
```

The script will:
1. Check for and install required dependencies
2. Prompt you to choose between local or container-based build
3. Provide instructions for building OneFileLinux

## Building with the Automated Script

### Local Build

After the setup script has prepared your environment:

```bash
# Build with default settings (minimal profile)
sbcl --load "main.lisp" -- 

# Build with specific profile
sbcl --load "main.lisp" -- --profile=standard

# Build with verbose output
sbcl --load "main.lisp" -- --profile=minimal --verbose
```

### Container Build with Docker

After the setup script has prepared your environment:

```bash
# Build the Docker image
docker build -t onefilelinux-builder:latest -f docker/Dockerfile .

# Run the build with default settings
docker run --rm -v $(pwd)/output:/output onefilelinux-builder:latest

# Build with specific profile
docker run --rm -v $(pwd)/output:/output onefilelinux-builder:latest --profile=standard

# Build with resource limits
docker run --rm --cpu 4 --memory 8g -v $(pwd)/output:/output onefilelinux-builder:latest
```

### Container Build with Podman

Podman is a daemonless container engine that's compatible with Docker commands:

```bash
# Build the container image
podman build -t onefilelinux-builder:latest -f docker/Dockerfile .

# Run the build with default settings
podman run --rm -v $(pwd)/output:/output onefilelinux-builder:latest

# Build with specific profile
podman run --rm -v $(pwd)/output:/output onefilelinux-builder:latest --profile=standard
```

## Manual SBCL Build (Advanced)

If you prefer to manually control the build process:

1. Start SBCL and load the system:

```bash
sbcl
```

2. Inside the SBCL REPL:

```lisp
(require :asdf)
(load "onefilelinux.asd")
(asdf:load-system :onefilelinux)
(in-package :onefilelinux.build)

;; Apply a profile
(onefilelinux.config:apply-profile "minimal")

;; Create a build context
(let ((context (make-build-context
               :config onefilelinux.config:*config*
               :working-dir (uiop:getcwd)
               :output-dir (onefilelinux.core:path-join (uiop:getcwd) "output")
               :verbose t)))
  (run-build context))
```

## Build Profiles

OneFileLinux supports different build profiles:

- **minimal**: Smallest possible size with basic functionality
- **standard**: Standard features for most use cases
- **full**: All features included

## Common Options

Both local and container builds support these options:

```
--profile=PROFILE    Build profile (minimal, standard, full)
--verbose            Enable verbose output
--help               Display help message
```

### Dry Run Mode

OneFileLinux supports a "dry run" mode which simulates the build process without executing resource-intensive steps:

```
--dry-run            Run in dry run mode (simulation only)
--dry-run-until=STEP Run until the specified step (prepare, get, chroot, conf, build)
--list-steps         List all available build steps
```

For example:

```bash
# Simulate full build process
./build.sh --dry-run

# Execute up to preparation step only
./build.sh --dry-run-until=prepare

# Execute up to source download only
./build.sh --dry-run-until=get

# Execute up to configuration step
./build.sh --dry-run-until=conf
```

See [Dry Run Feature](dry-run-feature.md) for more detailed information.

## Troubleshooting

### Common Issues

1. **Permissions Issues**:
   - Local build: Run SBCL with sudo for operations requiring root
   - Container build: Ensure your user has permission to use Docker/Podman

2. **Missing Libraries**:
   - Run the build.sh setup script again to install dependencies
   - If specific libraries are missing, run `./build.sh --verbose` to see detailed error messages

3. **Memory/CPU Limitations**:
   - For container builds, specify resource limits with --cpu and --memory options
   - For kernel build failures, try increasing available memory or use a container build

4. **Docker/Podman Group Membership**:
   - If you get permission denied errors, make sure your user is in the docker group
   - Run: `sudo usermod -aG docker $(whoami)` and then `newgrp docker`

### Using Dry Run for Debugging

The dry run feature is useful for troubleshooting build issues:

```bash
# Test system setup and dependencies
./build.sh --dry-run-until=prepare --verbose

# Debug source download issues
./build.sh --dry-run-until=get --verbose

# Isolate configuration problems
./build.sh --dry-run-until=conf --verbose
```

This allows you to focus on specific build stages without going through the entire build process.

## Output Files

After a successful build, you'll find the following files in the output directory:

- `onefilelinux-minimal.efi`: EFI executable for minimal profile
- `onefilelinux-standard.efi`: EFI executable for standard profile
- `onefilelinux-minimal.efi.xz`: Compressed EFI executable for minimal profile
- `onefilelinux-standard.efi.xz`: Compressed EFI executable for standard profile

## Next Steps

After building OneFileLinux, you can:

1. Boot it on real hardware using UEFI
2. Test in a virtual machine like QEMU or VirtualBox
3. Flash to a USB drive for portable use

Follow the user guide in the `docs` directory for more detailed usage instructions.