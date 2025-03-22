# OneFileLinux Prebuilt Docker Container

This document explains how to use the prebuilt OneFileLinux build environment container.

## Why Use a Prebuilt Container?

The prebuilt container offers several advantages:

1. **Instant Start**: No need to build the container from scratch each time
2. **All Dependencies Included**: All required build tools are pre-installed
3. **Resources Pre-Downloaded**: Alpine rootfs and other components are pre-cached
4. **CI/CD Ready**: Optimized for use in continuous integration pipelines
5. **Consistent Environment**: Identical build environment across all systems

## Quick Start

### Pull the Container

```bash
# Pull the latest container image
docker pull ghcr.io/onefilelinux/builder:latest
```

### Build OneFileLinux

```bash
# Create an output directory
mkdir -p output

# Run the container with the output directory mounted
docker run -it --rm \
  -v "$(pwd)/output:/onefilelinux/output" \
  ghcr.io/onefilelinux/builder:latest
```

### Build with Options

```bash
# Build minimal version
docker run -it --rm \
  -v "$(pwd)/output:/onefilelinux/output" \
  ghcr.io/onefilelinux/builder:latest ./build.sh --minimal

# Build full version with ZFS support
docker run -it --rm \
  -v "$(pwd)/output:/onefilelinux/output" \
  ghcr.io/onefilelinux/builder:latest ./build.sh --full --with-zfs
```

## Using with Docker Compose

Create a `docker-compose.yml` file:

```yaml
services:
  onefilelinux-builder:
    image: ghcr.io/onefilelinux/builder:latest
    container_name: onefilelinux-builder
    volumes:
      # Mount output directory
      - ./output:/onefilelinux/output:rw
    environment:
      # Configure build arguments here
      - BUILD_ARGS=--with-zfs --with-recovery-tools
      # Ensure feature variables propagate to all scripts
      - INCLUDE_ZFS=true
      - INCLUDE_MINIMAL_KERNEL=false
      - INCLUDE_NETWORK_TOOLS=true
      - INCLUDE_CRYPTO=true
    # Enable any required capabilities
    cap_add:
      - SYS_ADMIN  # Required for chroot operations
    # Enable privileged mode for complex operations like loop devices
    privileged: true
```

Then run:

```bash
docker-compose up
```

## Customizing the Build Environment

### Environment Variables

The container supports several environment variables for customization:

- `BUILD_ARGS`: Additional build arguments to pass to build.sh
- `INCLUDE_ZFS`: Include ZFS support (true/false)
- `INCLUDE_MINIMAL_KERNEL`: Use minimal kernel configuration (true/false)
- `INCLUDE_NETWORK_TOOLS`: Include network tools (true/false)
- `INCLUDE_CRYPTO`: Include crypto support (true/false)

Example:

```bash
docker run -it --rm \
  -v "$(pwd)/output:/onefilelinux/output" \
  -e INCLUDE_ZFS=false \
  -e INCLUDE_MINIMAL_KERNEL=true \
  ghcr.io/onefilelinux/builder:latest
```

### Interactive Shell

Access an interactive shell in the container:

```bash
docker run -it --rm \
  -v "$(pwd)/output:/onefilelinux/output" \
  ghcr.io/onefilelinux/builder:latest /bin/bash
```

## Using in CI/CD Pipelines

### GitHub Actions Example

```yaml
name: Build OneFileLinux

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  build:
    runs-on: ubuntu-latest
    container:
      image: ghcr.io/onefilelinux/builder:latest
      options: --privileged
    
    steps:
    - uses: actions/checkout@v3
      with:
        path: src
    
    - name: Build OneFileLinux
      run: |
        cd /onefilelinux/build
        ./build.sh --standard
      
    - name: Upload Artifacts
      uses: actions/upload-artifact@v3
      with:
        name: onefilelinux-efi
        path: /onefilelinux/output/*.efi
```

### GitLab CI Example

```yaml
build:
  image: 
    name: ghcr.io/onefilelinux/builder:latest
    entrypoint: [""]
  script:
    - cd /onefilelinux/build
    - ./build.sh --standard
  artifacts:
    paths:
      - /onefilelinux/output/*.efi
```

## Building Your Own Prebuilt Container

You can build and publish your own version of the container using the provided script:

```bash
# Make the script executable
chmod +x docker/build-container-image.sh

# Build the container locally
./docker/build-container-image.sh

# Build and push to a registry
./docker/build-container-image.sh --push --registry myregistry.com --username myuser --org myorg
```

## Troubleshooting

### Permission Issues

If you encounter permission issues with the output directory:

```bash
# Run the container as your user ID
docker run -it --rm \
  -v "$(pwd)/output:/onefilelinux/output" \
  -e HOST_UID=$(id -u) \
  -e HOST_GID=$(id -g) \
  ghcr.io/onefilelinux/builder:latest
```

### Resource Constraints

If the build fails due to resource constraints:

```bash
# Allocate more resources
docker run -it --rm \
  -v "$(pwd)/output:/onefilelinux/output" \
  --memory=8g --cpus=4 \
  ghcr.io/onefilelinux/builder:latest
```