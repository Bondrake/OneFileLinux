#!/bin/bash
#
# OneFileLinux Docker Container Image Builder
# This script builds and publishes a ready-to-use Docker image with the complete build environment

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# Define colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Banner function
print_banner() {
    echo -e "${BLUE}"
    echo "      ____________  "
    echo "    /|------------| "
    echo "   /_|  .---.     | "
    echo "  |    /     \    | "
    echo "  |    \.6-6./    | "
    echo "  |    /\`\_/\`\    | "
    echo "  |   //  _  \\\   | "
    echo "  |  | \     / |  | "
    echo "  | /\`\_\`>  <_/\`\ | "
    echo "  | \__/'---'\__/ | "
    echo "  |_______________| "
    echo "                    "
    echo -e "${GREEN}   OneFileLinux Container Image Builder  ${NC}"
    echo "----------------------------------------------------"
}

# Usage information
usage() {
    echo "Usage: $0 [options]"
    echo ""
    echo "Options:"
    echo "  -h, --help            Display this help message"
    echo "  -t, --tag TAG         Custom tag for the Docker image (default: latest)"
    echo "  -p, --pull            Pull the latest base image before building"
    echo "  --no-cache            Build the Docker image without using cache"
    echo "  --push                Push the image to registry after building"
    echo "  -r, --registry URL    Registry to push to (default: ghcr.io)"
    echo "  -u, --username USER   Username for registry authentication"
    echo "  -o, --org ORG         Organization name for the image (default: local)"
    echo ""
    echo "Examples:"
    echo "  $0                    Build with default settings as onefilelinux-builder:latest"
    echo "  $0 -t v1.0            Build with tag v1.0"
    echo "  $0 --push -o myorg    Build and push to myorg/onefilelinux-builder:latest"
    echo ""
}

# Parse command line arguments
TAG="latest"
PULL=false
NO_CACHE=false
PUSH=false
REGISTRY="ghcr.io"
USERNAME=""
ORG="local"

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            exit 0
            ;;
        -t|--tag)
            TAG=$2
            shift 2
            ;;
        -p|--pull)
            PULL=true
            shift
            ;;
        --no-cache)
            NO_CACHE=true
            shift
            ;;
        --push)
            PUSH=true
            shift
            ;;
        -r|--registry)
            REGISTRY=$2
            shift 2
            ;;
        -u|--username)
            USERNAME=$2
            shift 2
            ;;
        -o|--org)
            ORG=$2
            shift 2
            ;;
        *)
            echo -e "${RED}Error: Unknown option $1${NC}"
            usage
            exit 1
            ;;
    esac
done

# Print banner
print_banner

# Check if Docker is installed
if ! command -v docker &> /dev/null; then
    echo -e "${RED}Error: Docker is not installed or not in the PATH.${NC}"
    echo "Please install Docker first:"
    echo "  - macOS: https://docs.docker.com/desktop/install/mac/"
    echo "  - Linux: https://docs.docker.com/engine/install/"
    echo "  - Windows: https://docs.docker.com/desktop/install/windows/"
    exit 1
fi

# Check if Docker is running
if ! docker info &> /dev/null; then
    echo -e "${RED}Error: Docker daemon is not running.${NC}"
    echo "Please start Docker Desktop or the Docker service before continuing."
    exit 1
fi

# Set up image name
IMAGE_NAME="${ORG}/onefilelinux-builder:${TAG}"
if [ "$REGISTRY" != "" ] && [ "$REGISTRY" != "docker.io" ]; then
    FULL_IMAGE_NAME="${REGISTRY}/${IMAGE_NAME}"
else
    FULL_IMAGE_NAME="${IMAGE_NAME}"
fi

echo -e "${BLUE}[INFO]${NC} Building Docker image: ${FULL_IMAGE_NAME}"

# Pull the latest base image if requested
if [ "$PULL" = true ]; then
    echo -e "${BLUE}[INFO]${NC} Pulling latest Alpine base image..."
    docker pull alpine:latest
    echo -e "${GREEN}[SUCCESS]${NC} Base image updated."
fi

# Set up build arguments
BUILD_ARGS=""
if [ "$NO_CACHE" = true ]; then
    BUILD_ARGS="--no-cache"
fi

# Change to the docker directory
cd "$SCRIPT_DIR"

# Create an updated Dockerfile that includes the init scripts
echo -e "${BLUE}[INFO]${NC} Creating Dockerfile with init support..."

# Create a temporary modified Dockerfile
cp Dockerfile Dockerfile.ready

# Add init scripts by modifying the Dockerfile
# We'll insert setup code near the WORKDIR line
sed -i.bak '/WORKDIR \/onefilelinux/i \
# Pre-initialize the build environment \
RUN mkdir -p /onefilelinux/build /onefilelinux/output \
    /onefilelinux/.buildcache/sources \
    /onefilelinux/.buildcache/packages \
    /onefilelinux/.buildcache/ccache \
    /onefilelinux/.buildcache/build \
    && chown -R builder:builder /onefilelinux\n\
# Download Alpine minirootfs for reuse \
RUN mkdir -p /onefilelinux/.buildcache/sources && \\\n\
    cd /onefilelinux/.buildcache/sources && \\\n\
    wget -q https://dl-cdn.alpinelinux.org/alpine/v3.21/releases/x86_64/alpine-minirootfs-3.21.3-x86_64.tar.gz && \\\n\
    chown builder:builder alpine-minirootfs-3.21.3-x86_64.tar.gz\n\
# Copy build scripts \
COPY --chown=builder:builder ../build/*.sh /onefilelinux/build/\n\
# Copy tools \
COPY --chown=builder:builder ../build/tools/*.sh /onefilelinux/build/tools/\n\
# Copy zfiles \
COPY --chown=builder:builder ../build/zfiles /onefilelinux/build/zfiles/\n\
# Copy kernel configs \
COPY --chown=builder:builder ../build/kernel-configs /onefilelinux/build/kernel-configs/\n\
# Make scripts executable \
RUN chmod +x /onefilelinux/build/*.sh /onefilelinux/build/tools/*.sh\n\
# Pre-extract Alpine minirootfs to speed up initial build \
RUN cd /onefilelinux/build && \\\n\
    ln -sf /onefilelinux/.buildcache/sources/alpine-minirootfs-3.21.3-x86_64.tar.gz .\n
' Dockerfile.ready

# Build the Docker image
echo -e "${BLUE}[INFO]${NC} Building Docker image..."
docker build $BUILD_ARGS -t "${IMAGE_NAME}" -f Dockerfile.ready .
BUILD_RESULT=$?

# Clean up temporary Dockerfile
rm -f Dockerfile.ready Dockerfile.ready.bak

if [ $BUILD_RESULT -ne 0 ]; then
    echo -e "${RED}[ERROR]${NC} Docker image build failed."
    exit 1
fi

echo -e "${GREEN}[SUCCESS]${NC} Docker image built successfully: ${IMAGE_NAME}"

# Tag with registry name if pushing
if [ "$PUSH" = true ]; then
    echo -e "${BLUE}[INFO]${NC} Tagging image for registry: ${FULL_IMAGE_NAME}"
    docker tag "${IMAGE_NAME}" "${FULL_IMAGE_NAME}"
    
    # Login to registry if username provided
    if [ -n "$USERNAME" ]; then
        echo -e "${BLUE}[INFO]${NC} Logging in to registry ${REGISTRY}..."
        echo "Enter your registry password:"
        docker login "${REGISTRY}" -u "${USERNAME}"
        LOGIN_RESULT=$?
        
        if [ $LOGIN_RESULT -ne 0 ]; then
            echo -e "${RED}[ERROR]${NC} Failed to login to registry."
            exit 1
        fi
    fi
    
    echo -e "${BLUE}[INFO]${NC} Pushing image to registry: ${FULL_IMAGE_NAME}"
    docker push "${FULL_IMAGE_NAME}"
    PUSH_RESULT=$?
    
    if [ $PUSH_RESULT -ne 0 ]; then
        echo -e "${RED}[ERROR]${NC} Failed to push image to registry."
        exit 1
    fi
    
    echo -e "${GREEN}[SUCCESS]${NC} Image pushed to registry: ${FULL_IMAGE_NAME}"
fi

echo -e "${BLUE}[INFO]${NC} Creating README with usage instructions..."

cat > README.container.md << EOF
# OneFileLinux Builder Container

This container provides a complete, ready-to-use build environment for OneFileLinux.

## Quick Start

```bash
# Pull the image
docker pull ${FULL_IMAGE_NAME}

# Run with mounted volumes for output
docker run -it --rm \\
  -v \$(pwd)/output:/onefilelinux/output \\
  ${FULL_IMAGE_NAME}
```

## Advanced Usage

### Building with Custom Options

```bash
# Run with specific build options
docker run -it --rm \\
  -v \$(pwd)/output:/onefilelinux/output \\
  ${FULL_IMAGE_NAME} ./build.sh --minimal
```

### Interactive Shell

```bash
# Get an interactive shell in the container
docker run -it --rm \\
  -v \$(pwd)/output:/onefilelinux/output \\
  ${FULL_IMAGE_NAME} /bin/bash
```

### Custom Build Steps

```bash
# Run only specific build steps
docker run -it --rm \\
  -v \$(pwd)/output:/onefilelinux/output \\
  ${FULL_IMAGE_NAME} /bin/bash -c "cd /onefilelinux/build && ./01_get.sh && ./02_chrootandinstall.sh"
```

## Environment Variables

- \`BUILD_ARGS\`: Additional build arguments to pass to build.sh
- \`INCLUDE_ZFS\`: Include ZFS support (true/false)
- \`INCLUDE_MINIMAL_KERNEL\`: Use minimal kernel configuration (true/false)
- \`INCLUDE_NETWORK_TOOLS\`: Include network tools (true/false)
- \`INCLUDE_CRYPTO\`: Include crypto support (true/false)

Example:

```bash
docker run -it --rm \\
  -v \$(pwd)/output:/onefilelinux/output \\
  -e INCLUDE_ZFS=false \\
  -e INCLUDE_MINIMAL_KERNEL=true \\
  ${FULL_IMAGE_NAME}
```

## Resource Settings

The container automatically adapts to available resources, but you can specify limits:

```bash
docker run -it --rm \\
  -v \$(pwd)/output:/onefilelinux/output \\
  --memory=4g --cpus=2 \\
  ${FULL_IMAGE_NAME}
```

## Container Details

This container includes:
- All build dependencies pre-installed
- Alpine Linux rootfs pre-downloaded
- Build scripts and configuration files
- CCache for faster rebuilds
EOF

echo -e "${GREEN}[SUCCESS]${NC} README created: README.container.md"

# Final summary
echo -e "${GREEN}[COMPLETE]${NC} OneFileLinux builder container image created successfully."
echo -e "${BLUE}[INFO]${NC} Container image details:"
echo "  - Image name: ${FULL_IMAGE_NAME}"
echo "  - Ready to use with: docker run -it --rm -v \$(pwd)/output:/onefilelinux/output ${FULL_IMAGE_NAME}"
echo ""
echo -e "${BLUE}[INFO]${NC} To use this container in CI/CD pipelines:"
echo "  1. Add this to your workflow:"
echo "     runs-on: ubuntu-latest"
echo "     container: ${FULL_IMAGE_NAME}"
echo "  2. Mount output directory for artifacts"
echo ""
echo -e "${BLUE}[INFO]${NC} See README.container.md for detailed usage instructions."