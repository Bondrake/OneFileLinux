#!/bin/bash
#
# OneFileLinux Docker Build Script
# This script launches the OneFileLinux build inside a Docker container

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
    echo -e "${GREEN}   OneFileLinux Docker Builder  ${NC}"
    echo "----------------------------------------------------"
}

# Usage information
usage() {
    echo "Usage: $0 [options]"
    echo ""
    echo "Options:"
    echo "  -h, --help            Display this help message"
    echo "  -c, --clean           Clean the Docker environment before building"
    echo "  -v, --verbose         Enable verbose output"
    echo "  -b, --build-args ARG  Pass build arguments to the build script (uses the -- passthrough mechanism)"
    echo "  -e, --env-file FILE   Specify a custom .env file"
    echo "  -i, --interactive     Run in interactive mode (shell inside container)"
    echo "  -p, --pull            Pull the latest base image before building"
    echo "  --no-cache            Build the Docker image without using cache"
    echo "  --max-resources       Use maximum available system resources"
    echo "  --balanced-resources  Use balanced system resources (default)"
    echo "  --min-resources       Use minimal system resources"
    echo "  --make-verbose        Enable verbose make output (V=1)"
    echo "  --make-quiet          Use quiet make output (V=0, default)"
    echo ""
    echo "Examples:"
    echo "  $0                    Build with default settings"
    echo "  $0 -c                 Clean and rebuild"
    echo "  $0 -b \"--profile=minimal\"    Build with minimal profile"
    echo "  $0 -b \"--profile=standard\"   Build with standard profile (default)"
    echo "  $0 -b \"--profile=full\"       Build with full profile"
    echo "  $0 -i                 Launch interactive shell in the container"
    echo "  $0 --max-resources    Use maximum available system resources"
    echo "  $0 --make-verbose     Build with verbose make output (V=1)"
    echo "  $0 --make-quiet       Build with quiet make output (V=0, default)"
    echo ""
}

# Parse command line arguments
CLEAN=false
VERBOSE=false
BUILD_ARGS=""
ENV_FILE=".env"
INTERACTIVE=false
PULL=false
NO_CACHE=false
RESOURCES="balanced"
MAKE_VERBOSE=0

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            exit 0
            ;;
        -c|--clean)
            CLEAN=true
            shift
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -b|--build-args)
            BUILD_ARGS=$2
            shift 2
            ;;
        -e|--env-file)
            ENV_FILE=$2
            shift 2
            ;;
        -i|--interactive)
            INTERACTIVE=true
            shift
            ;;
        -p|--pull)
            PULL=true
            shift
            ;;
        --no-cache)
            NO_CACHE=true
            shift
            ;;
        --max-resources)
            RESOURCES="max"
            shift
            ;;
        --balanced-resources)
            RESOURCES="balanced"
            shift
            ;;
        --min-resources)
            RESOURCES="min"
            shift
            ;;
        --make-verbose)
            MAKE_VERBOSE=1
            # Append to existing build args if they exist
            if [[ -n "$BUILD_ARGS" ]]; then
                BUILD_ARGS="$BUILD_ARGS --make-verbose"
            else
                BUILD_ARGS="--make-verbose"
            fi
            shift
            ;;
        --make-quiet)
            MAKE_VERBOSE=0
            # Append to existing build args if they exist
            if [[ -n "$BUILD_ARGS" ]]; then
                BUILD_ARGS="$BUILD_ARGS --make-quiet"
            else
                BUILD_ARGS="--make-quiet"
            fi
            shift
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

# Set the host user ID for consistent file ownership
export HOST_UID=$(id -u)
export HOST_GID=$(id -g)

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

# Check if docker-compose is installed
if ! command -v docker-compose &> /dev/null; then
    echo -e "${YELLOW}Warning: docker-compose not found, falling back to Docker Compose plugin.${NC}"
    COMPOSE_CMD="docker compose"
else
    COMPOSE_CMD="docker-compose"
fi

# Make auto-resources.sh executable
chmod +x "$SCRIPT_DIR/auto-resources.sh"

# Determine resource allocation based on selected profile
echo -e "${BLUE}[INFO]${NC} Detecting system resources..."
case $RESOURCES in
    max)
        # Leave minimal resources for the host
        source "$SCRIPT_DIR/auto-resources.sh" --env > /dev/null
        # Override default resource limits with detected values
        MIN_FREE_MEM_GB=2
        MIN_FREE_CPUS=1
        # Calculate with minimal reserves - memory is already in g format (like "16g")
        MEMORY_VALUE=$(echo $DOCKER_MEMORY | sed 's/g//')
        AVAIL_MEM_GB=$((MEMORY_VALUE - MIN_FREE_MEM_GB))
        DOCKER_MEMORY="${AVAIL_MEM_GB}g"
        echo -e "${BLUE}[INFO]${NC} Using maximum available resources: $DOCKER_MEMORY RAM, $DOCKER_CPUS CPU cores"
        ;;
    balanced)
        # Default balanced mode
        source "$SCRIPT_DIR/auto-resources.sh" --env > /dev/null
        echo -e "${BLUE}[INFO]${NC} Using balanced resources: $DOCKER_MEMORY RAM, $DOCKER_CPUS CPU cores"
        ;;
    min)
        # Minimal resource usage
        export DOCKER_MEMORY="4g"
        export DOCKER_CPUS="2"
        export BUILD_FLAGS="--jobs=2 --use-swap"
        echo -e "${BLUE}[INFO]${NC} Using minimal resources: $DOCKER_MEMORY RAM, $DOCKER_CPUS CPU cores"
        ;;
esac

# Append resource flags to build arguments if not already specified
if [[ "$BUILD_ARGS" != *"--jobs="* ]] && [[ -n "$BUILD_FLAGS" ]]; then
    BUILD_ARGS="$BUILD_ARGS $BUILD_FLAGS"
fi

# Create .env file for docker-compose
cat > "$SCRIPT_DIR/.env" << EOF
# Auto-generated environment file for OneFileLinux build
# Generated on $(date)

# Resource limits
DOCKER_MEMORY=$DOCKER_MEMORY
DOCKER_CPUS=$DOCKER_CPUS

# Build arguments
BUILD_ARGS=$BUILD_ARGS

# User mapping
HOST_UID=$HOST_UID
HOST_GID=$HOST_GID
EOF

# Clean if requested
if [ "$CLEAN" = true ]; then
    echo -e "${BLUE}[INFO]${NC} Cleaning up Docker environment..."
    cd "$SCRIPT_DIR"
    $COMPOSE_CMD down -v --rmi all
    echo -e "${GREEN}[SUCCESS]${NC} Docker environment cleaned."
fi

# Create output directory if it doesn't exist
mkdir -p "$PROJECT_DIR/output"

# Set verbose mode if requested
if [ "$VERBOSE" = true ]; then
    export VERBOSE=true
    echo -e "${BLUE}[INFO]${NC} Verbose mode enabled."
fi

# Pull the latest base image if requested
if [ "$PULL" = true ]; then
    echo -e "${BLUE}[INFO]${NC} Pulling latest Alpine base image..."
    docker pull alpine:latest
    echo -e "${GREEN}[SUCCESS]${NC} Base image updated."
fi

# Build options for docker-compose
BUILD_OPTS=""
if [ "$NO_CACHE" = true ]; then
    BUILD_OPTS="--build --no-cache"
    echo -e "${BLUE}[INFO]${NC} Building without cache."
else
    BUILD_OPTS="--build"
fi

# Check if Docker Buildx is available for better performance
if docker buildx version &>/dev/null; then
    echo -e "${BLUE}[INFO]${NC} Using Docker Buildx for improved build performance."
    export DOCKER_BUILDKIT=1
    export COMPOSE_DOCKER_CLI_BUILD=1
else
    echo -e "${YELLOW}[WARNING]${NC} Docker Buildx not detected. Build performance may be reduced."
fi

# Change to the docker directory
cd "$SCRIPT_DIR"

# Show resource allocation
echo -e "${BLUE}[INFO]${NC} Docker container resource allocation:"
echo -e "${BLUE}[INFO]${NC} - Memory: $DOCKER_MEMORY"
echo -e "${BLUE}[INFO]${NC} - CPUs: $DOCKER_CPUS"
echo -e "${BLUE}[INFO]${NC} - Build flags: $BUILD_ARGS"

# Run in interactive mode or normal build mode
# Record build start time
BUILD_START_TIME=$(date +%s)

if [ "$INTERACTIVE" = true ]; then
    echo -e "${BLUE}[INFO]${NC} Starting interactive shell in container..."
    $COMPOSE_CMD run --rm $BUILD_OPTS onefilelinux-builder /bin/bash
else
    echo -e "${BLUE}[INFO]${NC} Starting OneFileLinux build in container..."
    
    # Print timestamp
    echo -e "${BLUE}[INFO]${NC} Build started at $(date)"
    
    # Run the container with detailed debugging
    $COMPOSE_CMD up $BUILD_OPTS --remove-orphans 
    COMPOSE_EXIT_CODE=$?
    echo -e "${BLUE}[DEBUG]${NC} Docker Compose exit code: $COMPOSE_EXIT_CODE"
    
    # Check the actual container exit code which might be different
    CONTAINER_EXIT=$(docker inspect --format='{{.State.ExitCode}}' onefilelinux-builder 2>/dev/null || echo "unknown")
    echo -e "${BLUE}[DEBUG]${NC} Container onefilelinux-builder exit code: $CONTAINER_EXIT"
    
    # Check for output files (handling all naming patterns)
    echo -e "${BLUE}[INFO]${NC} Checking for build output files..."
    
    # List all EFI files in the output directory
    output_files=$(find "$PROJECT_DIR/output" -name "*.efi" | sort)
    
    if [ -n "$output_files" ]; then
        echo -e "${GREEN}[SUCCESS]${NC} OneFileLinux build completed successfully!"
        
        # Display each output file
        echo -e "${BLUE}[INFO]${NC} Output files created:"
        for efi_file in $output_files; do
            FILE_SIZE=$(du -h "$efi_file" | cut -f1)
            file_basename=$(basename "$efi_file")
            echo -e "  - ${GREEN}$file_basename${NC} (Size: $FILE_SIZE)"
        done
        
        # Select the main file to display for backward compatibility messaging
        # Prioritize profile-specific files first, then fallback to generic name
        if echo "$output_files" | grep -q "custom"; then
            MAIN_FILE=$(echo "$output_files" | grep "custom" | head -1)
        elif echo "$output_files" | grep -q "standard"; then
            MAIN_FILE=$(echo "$output_files" | grep "standard" | head -1)
        elif echo "$output_files" | grep -q "minimal"; then
            MAIN_FILE=$(echo "$output_files" | grep "minimal" | head -1)
        elif echo "$output_files" | grep -q "full"; then
            MAIN_FILE=$(echo "$output_files" | grep "full" | head -1)
        else
            # Fallback to first file (likely OneFileLinux.efi)
            MAIN_FILE=$(echo "$output_files" | head -1)
        fi
        
        echo -e "${BLUE}[INFO]${NC} Primary output file: $MAIN_FILE"
        
        # Display build timing information if available
        TIMING_LOG="$PROJECT_DIR/build/build_timing.log"
        if [ -f "$TIMING_LOG" ]; then
            echo -e "${BLUE}[INFO]${NC} Build timing information:"
            echo "=============================================================="
            cat "$TIMING_LOG"
            echo "=============================================================="
            echo -e "${BLUE}[INFO]${NC} Full timing log saved to: $TIMING_LOG"
        fi
    else
        # No EFI files found - check for errors
        
        # Check if compose itself failed 
        if [ $COMPOSE_EXIT_CODE -ne 0 ]; then
            echo -e "${RED}[ERROR]${NC} Docker Compose execution failed with exit code $COMPOSE_EXIT_CODE."
            echo -e "${RED}[ERROR]${NC} OneFileLinux build failed. See container logs for details."
            exit 1
        else
            # Compose succeeded but no output file - container likely exited with error
            echo -e "${RED}[ERROR]${NC} Build process failed. Container exited without creating any EFI files."
            echo -e "${YELLOW}[WARNING]${NC} Check container logs for details."
            
            # Check what files were actually created in the output directory
            echo -e "${BLUE}[INFO]${NC} Files in output directory:"
            ls -la "$PROJECT_DIR/output/"
            
            # Look for build errors in container logs
            if $COMPOSE_CMD logs | grep -i "error\|fail\|fatal" > /dev/null; then
                echo -e "${YELLOW}[WARNING]${NC} Found error messages in container logs:"
                $COMPOSE_CMD logs | grep -i "error\|fail\|fatal" | tail -n 10
            fi
            exit 1
        fi
    fi
fi

# Calculate and display the total build time
BUILD_END_TIME=$(date +%s)
BUILD_DURATION=$((BUILD_END_TIME - BUILD_START_TIME))
BUILD_MINUTES=$((BUILD_DURATION / 60))
BUILD_SECONDS=$((BUILD_DURATION % 60))

echo -e "${BLUE}[INFO]${NC} Total build time: ${BUILD_MINUTES}m ${BUILD_SECONDS}s"
echo -e "${BLUE}[INFO]${NC} Process complete."