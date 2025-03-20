# OneFileLinux Build System

This directory contains the build system for OneFileLinux. For complete documentation, see the [User Guide](../docs/USER_GUIDE.md).

## Quick Start

```bash
# Build with default options
sudo ./build.sh

# Build without ZFS support (smaller image)
sudo ./build.sh --without-zfs

# Build with minimal components
sudo ./build.sh --minimal

# Build with all advanced package groups
sudo ./build.sh --full
```

## Directory Structure

- `00_prepare.sh` - Prepares build environment and installs dependencies
- `01_get.sh` - Downloads Alpine Linux, Linux kernel, and ZFS sources
- `02_chrootandinstall.sh` - Sets up chroot and installs packages
- `03_conf.sh` - Configures system services and settings
- `04_build.sh` - Builds kernel and creates EFI executable
- `99_cleanup.sh` - Removes build artifacts
- `build.sh` - Unified build script that orchestrates the entire process
- `80_common.sh` - Basic utilities, logging functions, and environment detection
- `81_error_handling.sh` - Error handling, recovery options, and prerequisite checks
- `82_build_helper.sh` - File operations, environment-specific functions, and build utilities
- `83_config_helper.sh` - Configuration utilities, option parsing, and config file management
- `build.conf` - Saved build configuration (generated on first run)
- `zfiles/` - Configuration files for the build
  - `kernel-minimal.config` - Minimal kernel configuration
  - `init` - System initialization script
  - `interfaces` - Network configuration
  - `onefilelinux-tui` - Text-based user interface script
  - `profile` - System profile
  - `resolv.conf` - DNS configuration
  - `shadow` - User accounts configuration

## Configuration

The build process can be customized using command-line flags. Run `./build.sh --help` for a complete list of options.

## Build Options

For a comprehensive list of build options, see the [User Guide](../docs/USER_GUIDE.md#detailed-build-script-options).

## Advanced Usage

```bash
# Build with verbose output
./build.sh -v

# Run only specific step
./build.sh build

# Clean start and clean end
./build.sh -c -C

# Skip preparation step
./build.sh -s build

# Save current configuration as default
./build.sh --save-config

# Display current build configuration
./build.sh --show-config
```

## Troubleshooting

- If the build fails, check the error messages for specific issues
- Use `-v` flag for verbose output to help identify problems
- Check disk space - at least 10GB free space is recommended
- Ensure all dependencies are installed
- For build errors in specific steps, try running that step manually
- Check log files in the build directory for detailed error information

For detailed build instructions, advanced configuration options, and component descriptions, see the [User Guide](../docs/USER_GUIDE.md).
