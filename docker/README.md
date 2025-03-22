# OneFileLinux Docker Build Environment

This directory contains files for building OneFileLinux using Docker, providing an isolated and consistent build environment with all dependencies pre-installed.

## Quick Start

```bash
# Build the Docker image
docker build -t onefilelinux-builder .

# Run a build with the minimal profile
docker run -v $(pwd)/output:/output onefilelinux-builder --profile minimal

# Run a build with the standard profile
docker run -v $(pwd)/output:/output onefilelinux-builder --profile standard
```

## Docker Image Contents

The Docker image includes:

- Ubuntu 24.04 base system
- SBCL (Steel Bank Common Lisp)
- Quicklisp package manager
- All required build dependencies
- The OneFileLinux build system

## Docker Build Options

The Docker build supports various command-line options:

```
--profile PROFILE    Build profile to use (minimal, standard, full)
--cpu NUM            Number of CPU cores to use
--memory NUM         Memory limit in MB
--output-dir DIR     Directory for build output
--github-actions     Enable GitHub Actions integration
--help, -h           Display help message
```

## Using Docker Compose

For convenience, you can use Docker Compose:

1. Create a `docker-compose.yml` file:

```yaml
version: '3'
services:
  builder:
    build: ./docker
    volumes:
      - ./output:/output
    command: --profile minimal
```

2. Run the build:

```bash
docker-compose up
```

## Environment Variables

You can customize the build using environment variables:

- `ONEFILELINUX_LOG_LEVEL`: Set logging level (debug, info, warning, error)
- `ONEFILELINUX_PROFILE`: Default build profile (minimal, standard, full)
- `ONEFILELINUX_PRESERVE_WORK`: Set to "true" to preserve intermediate files

## Resource Usage

The Docker build automatically detects available resources and adjusts build parameters accordingly. You can override these with the `--cpu` and `--memory` options.

## Output Files

After a successful build, you'll find the following files in the mounted output directory:

- `onefilelinux-minimal.efi`: EFI executable for minimal profile
- `onefilelinux-standard.efi`: EFI executable for standard profile
- `onefilelinux-minimal.efi.xz`: Compressed EFI executable for minimal profile
- `onefilelinux-standard.efi.xz`: Compressed EFI executable for standard profile

## GitHub Actions Integration

The Docker build environment includes special support for GitHub Actions CI/CD pipelines. Enable this with the `--github-actions` flag when running in a GitHub Actions workflow.

This provides:
- Proper GitHub Actions annotations for warnings and errors
- Output variables for build results
- Build logs formatted for GitHub

## Troubleshooting

### Common Issues

1. **Permission Problems**
   
   If you encounter permission issues with the output directory:
   
   ```bash
   docker run -v $(pwd)/output:/output --user $(id -u):$(id -g) onefilelinux-builder
   ```

2. **Resource Constraints**

   If the build fails due to resource constraints, try limiting resource usage:
   
   ```bash
   docker run -v $(pwd)/output:/output onefilelinux-builder --cpu 2 --memory 4096
   ```

3. **Docker Image Build Failures**

   If the Docker image fails to build, try:
   
   ```bash
   docker build --no-cache -t onefilelinux-builder .
   ```

4. **Debugging Build Issues**

   To debug build issues, enable debugging output:
   
   ```bash
   docker run -v $(pwd)/output:/output -e ONEFILELINUX_LOG_LEVEL=debug onefilelinux-builder
   ```

   Or get a shell in the container:
   
   ```bash
   docker run -it --entrypoint /bin/bash onefilelinux-builder
   ```

## Advanced Usage

### Custom Build Profiles

You can create custom build profiles by mounting a configuration file:

```bash
docker run -v $(pwd)/output:/output -v $(pwd)/my-profile.lisp:/build/profiles/custom.lisp onefilelinux-builder --profile custom
```

### Caching Build Artifacts

For faster rebuilds, you can cache build artifacts:

```bash
docker run -v $(pwd)/output:/output -v $(pwd)/cache:/build/cache onefilelinux-builder --use-cache
```

### Building Multiple Profiles

To build multiple profiles in one command:

```bash
docker run -v $(pwd)/output:/output onefilelinux-builder bash -c "onefilelinux-build --profile minimal && onefilelinux-build --profile standard"
```