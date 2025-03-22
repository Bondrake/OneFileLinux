# OneFileLinux

OneFileLinux is a specialized Linux distribution designed to fit in a single EFI executable file, making it portable, minimal, and easy to boot on modern hardware.

## Lisp Rewrite

This version has been completely rewritten in Common Lisp, applying modern software architecture principles and design patterns to create a more maintainable, extensible, and robust build system.

## Goals

1. **Improved Architecture**: Apply proper design patterns and architectural principles
2. **Better Orthogonality**: Reduce coupling between components and improve separation of concerns
3. **Enhanced Extensibility**: Make the system more modular and easier to extend
4. **Increased Maintainability**: Improve code organization, naming conventions, and documentation
5. **Cross-Platform Building**: Support for building on Linux, macOS, and Windows

## Project Structure

```
/
├── README.md                  # This file
├── core.lisp                  # Core utilities and common functions
├── config.lisp                # Configuration system
├── build.lisp                 # Build orchestration system
├── main.lisp                  # Main entry point
├── onefilelinux.asd           # ASDF system definition
├── build.sh                   # Bootstrap shell script
├── build-config.sh            # Configuration for bootstrap
├── steps/                     # Build steps implementation
│   ├── prepare.lisp           # Environment preparation step
│   ├── get.lisp               # Source acquisition step
│   ├── chrootandinstall.lisp  # Chroot and package installation step
│   ├── conf.lisp              # System configuration step
│   └── build.lisp             # Final build and EFI creation step
├── kernel/                    # Kernel-related utilities
│   └── config-utils.lisp      # Kernel configuration utilities
├── package/                   # Package management utilities
│   └── apk-builder.lisp       # Custom APK builder
├── docker/                    # Docker integration
│   ├── Dockerfile             # Container definition
│   ├── build-onefilelinux.lisp # Docker build launcher
│   └── auto-resources.lisp    # Resource detection and allocation
├── github/                    # GitHub Actions integration
│   └── actions-helper.lisp    # GitHub Actions utilities
├── tests/                     # Test suite
│   └── run-tests.lisp         # Test runner
└── docs/                      # Documentation
    ├── build-instructions.md  # How to build OneFileLinux
    ├── build-automation.md    # Build automation documentation
    └── ...                    # Additional documentation
```

## Design Patterns Used

### Command Pattern
Used for build steps, where each step has a clear lifecycle (prepare, execute, cleanup) and encapsulates its own functionality.

### Strategy Pattern
Applied for interchangeable algorithms like resource detection, package resolution, and feature configuration.

### Builder Pattern
Used in the configuration system for constructing complex configurations in a step-by-step process.

### Observer Pattern
Applied to build events and logging, allowing components to subscribe to and react to system events without coupling.

### Factory Method Pattern
Used for creating build step instances with proper initialization and registration.

### Dependency Injection
Reducing reliance on global state by explicitly passing context objects between components.

## Key Improvements

1. **Clear Component Boundaries**: Components have well-defined responsibilities and interfaces
2. **Reduced Global State**: Explicit context objects instead of global variables
3. **Proper Error Handling**: Structured condition hierarchy with specialized error types
4. **Consistent Logging**: Centralized logging system with configurable levels and destinations
5. **Improved Resource Management**: Better handling of system resources and cleanup operations
6. **Configuration Schema**: Validation and organization of configuration parameters
7. **Modular Build Steps**: Each build step is a self-contained component with clear lifecycle
8. **Dry Run Capability**: Support for simulating the build process without resource-intensive steps
9. **Testability**: Design supports unit testing with reduced dependencies and clear interfaces
10. **Cross-Platform Support**: Build on Linux (direct or container), macOS (container), Windows (container)

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

For more detailed build instructions, see [Build Instructions](docs/build-instructions.md). 

For information about the dry run feature, see [Dry Run Feature](docs/dry-run-feature.md).

## Build Process

The build process follows the same sequence as the original implementation:

1. **Prepare**: Environment preparation and system detection
2. **Get**: Download and extract Alpine Linux and kernel sources
3. **Chroot and Install**: Set up chroot environment and install packages
4. **Configure**: System configuration and service setup
5. **Build**: Kernel building, APK packaging, and EFI creation

Each step is implemented as a separate component following the Command pattern, with clear prepare, execute, and cleanup phases.

## Docker Integration

The Docker integration provides improved resource detection and allocation, with better handling of container limitations. The GitHub Actions integration facilitates CI/CD workflows with proper reporting and annotations.

## License

OneFileLinux is licensed under the MIT License. See the LICENSE file for details.