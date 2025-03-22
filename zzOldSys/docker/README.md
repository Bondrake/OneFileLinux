# OneFileLinux Docker Build System

This directory contains files for building OneFileLinux using Docker, providing a consistent build environment regardless of host system.

## Features

- Isolated build environment with all dependencies pre-installed
- Consistent builds across different host platforms (Linux, macOS, Windows)
- Efficient build caching for faster rebuilds
- Proper permission handling between container and host
- Support for all OneFileLinux build options
- Volume mounting for persistent build artifacts
- Easy to use command-line interface

## Prerequisites

- Docker (Docker Desktop for macOS/Windows, Docker Engine for Linux)
- Docker Compose (included with Docker Desktop or install separately)
- At least 4GB of free memory
- At least 10GB of free disk space

## Quick Start

1. Make the build script executable:
   ```bash
   chmod +x build-onefilelinux.sh
   ```

2. Run the build with default settings:
   ```bash
   ./build-onefilelinux.sh
   ```

3. For a full build with all features:
   ```bash
   ./build-onefilelinux.sh -b "--full"
   ```

4. For a minimal build:
   ```bash
   ./build-onefilelinux.sh -b "--minimal"
   ```

## Configuration

You can configure the build by:

1. Creating a `.env` file (copy from `.env.example`) and modifying variables
2. Using command-line arguments with the `-b` or `--build-args` option

Example `.env` file:
```
BUILD_ARGS=--minimal --use-cache
HOST_UID=1000
HOST_GID=1000
```

## Command Line Options

The `build-onefilelinux.sh` script supports several options:

```
Usage: ./build-onefilelinux.sh [options]

Options:
  -h, --help            Display this help message
  -c, --clean           Clean the Docker environment before building
  -v, --verbose         Enable verbose output
  -b, --build-args ARG  Pass build arguments to the build script
  -e, --env-file FILE   Specify a custom .env file
  -i, --interactive     Run in interactive mode (shell inside container)
  -p, --pull            Pull the latest base image before building
  --no-cache            Build the Docker image without using cache
```

## Directory Structure

- `Dockerfile`: Defines the containerized build environment
- `docker-compose.yml`: Configuration for Docker Compose
- `build-onefilelinux.sh`: Main build script to interact with the Docker environment
- `entrypoint.sh`: Docker container entry point that bootstraps the build process 
- `auto-resources.sh`: Helper script to automatically detect and configure system resources
- `.env.example`: Example environment variable file

## Build Artifacts

After a successful build, the output file (`OneFileLinux.efi`) will be placed in the `../output/` directory relative to this directory.

## Troubleshooting

### Build Fails with Permission Errors

If you encounter permission errors, try:
1. Ensuring the `HOST_UID` and `HOST_GID` are set correctly in the `.env` file
2. Running the build script with the `-c` option to clean the environment
3. Running Docker with appropriate privileges

### Container Runs Out of Memory

If the build process runs out of memory:
1. Increase Docker's memory allocation in Docker Desktop settings
2. Add `DOCKER_MEMORY=8g` to your `.env` file
3. Use the `--use-swap` build option to enable swap inside the build

### Slow Build Performance

To improve build performance:
1. Use the `--use-cache` build option (enabled by default)
2. Increase Docker's CPU allocation
3. Use SSD storage for Docker volumes

## Advanced Usage

### Interactive Mode

You can enter an interactive shell in the container for debugging or manual builds:

```bash
./build-onefilelinux.sh -i
```

### Custom Build Steps

To run specific build steps:

```bash
./build-onefilelinux.sh -i
# Inside the container:
cd /onefilelinux/build
./build.sh get  # Only run the download step
./build.sh build  # Only run the build step
```

### Clearing the Cache

To clear the build cache:

```bash
./build-onefilelinux.sh -c
```

## Integration with CI/CD

This Docker build system can be easily integrated with CI/CD pipelines like GitHub Actions. See the `.github/workflows` directory for examples.

## Build System Architecture

The Docker build system has been integrated with the standardized library-based build system:

1. **Library-Based Build**: The Docker entrypoint automatically detects and uses the library system (80-89 range):
   - `80_common.sh`: Logging, banners, environment detection
   - `81_error_handling.sh`: Error handling and prerequisite checks
   - `82_build_helper.sh`: Docker-specific functions for extraction and file handling
   - `83_config_helper.sh`: Configuration management

2. **Fallback Mode**: If the library files are not detected, the system falls back to the legacy build process using build.sh directly

3. **Environment Detection**: The container automatically marks `IN_DOCKER_CONTAINER=true` for proper environment-specific handling