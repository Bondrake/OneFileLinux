#!/bin/bash
#
# Synchronize dependencies between local build and Docker environment
# This script ensures the same dependencies are used in both environments
#

# Get the script's directory and the project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Source the common library
source "$PROJECT_ROOT/build/80_common.sh"

# Define color codes for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Print usage information
usage() {
    echo "Usage: $0 [options]"
    echo ""
    echo "Options:"
    echo "  -h, --help           Display this help message"
    echo "  -v, --verbose        Enable verbose output"
    echo "  -d, --dry-run        Print changes without applying them"
    echo "  -f, --force          Force update even if files are in sync"
    echo ""
    echo "Description:"
    echo "  This script synchronizes the dependencies between the local build system"
    echo "  and the Docker environment, ensuring consistency across all build paths."
    echo ""
    echo "Examples:"
    echo "  $0                  Synchronize dependencies"
    echo "  $0 --dry-run        Show what would be synchronized without making changes"
    echo "  $0 --verbose        Display detailed information during synchronization"
    echo ""
}

# Parse command line arguments
VERBOSE=false
DRY_RUN=false
FORCE=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            exit 0
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -d|--dry-run)
            DRY_RUN=true
            shift
            ;;
        -f|--force)
            FORCE=true
            shift
            ;;
        *)
            echo -e "${RED}Error: Unknown option $1${NC}"
            usage
            exit 1
            ;;
    esac
done

# Function to log messages with optional verbosity
log() {
    local level=$1
    local message=$2
    
    case "$level" in
        "INFO")
            if [ "$VERBOSE" = true ]; then
                echo -e "${BLUE}[INFO]${NC} $message"
            fi
            ;;
        "WARNING")
            echo -e "${YELLOW}[WARNING]${NC} $message"
            ;;
        "ERROR")
            echo -e "${RED}[ERROR]${NC} $message"
            ;;
        "SUCCESS")
            echo -e "${GREEN}[SUCCESS]${NC} $message"
            ;;
        *)
            echo -e "$message"
            ;;
    esac
}

# Check if required files exist
check_files() {
    local missing=false

    if [ ! -f "$PROJECT_ROOT/build/85_dependency_helper.sh" ]; then
        log "ERROR" "Dependency helper not found: $PROJECT_ROOT/build/85_dependency_helper.sh"
        missing=true
    fi

    if [ ! -f "$PROJECT_ROOT/docker/Dockerfile" ]; then
        log "ERROR" "Dockerfile not found: $PROJECT_ROOT/docker/Dockerfile"
        missing=true
    fi

    if [ ! -f "$PROJECT_ROOT/build/00_prepare.sh" ]; then
        log "ERROR" "Prepare script not found: $PROJECT_ROOT/build/00_prepare.sh"
        missing=true
    fi

    if [ "$missing" = true ]; then
        log "ERROR" "Required files are missing. Aborting."
        exit 1
    fi
}

# Function to synchronize dependencies
sync_dependencies() {
    # Source the dependency helper
    source "$PROJECT_ROOT/build/85_dependency_helper.sh"

    log "INFO" "Getting Docker dependencies..."
    local docker_deps=$(get_docker_dependencies)
    
    if [ "$VERBOSE" = true ]; then
        log "INFO" "Docker dependencies:"
        for pkg in $docker_deps; do
            log "INFO" "  - $pkg"
        done
    fi

    # Dockerfile path
    local dockerfile_path="$PROJECT_ROOT/docker/Dockerfile"
    
    # Make Dockerfile update
    log "INFO" "Updating Dockerfile dependencies..."
    
    if [ "$DRY_RUN" = true ]; then
        log "INFO" "Dry run mode: would update $dockerfile_path"
    else
        # Update the Dockerfile with the new dependencies
        update_dockerfile_dependencies "$dockerfile_path"
        if [ $? -ne 0 ]; then
            log "ERROR" "Failed to update Dockerfile dependencies"
            exit 1
        fi
        log "SUCCESS" "Dockerfile dependencies updated successfully"
    fi
    
    log "INFO" "Generating example install commands for common distributions..."
    
    # Generate example commands for common distributions
    local distros=("Ubuntu" "Debian" "Fedora" "Arch Linux" "Alpine Linux")
    
    echo ""
    echo "Example install commands for various distributions:"
    echo "==================================================="
    for distro in "${distros[@]}"; do
        local install_cmd=$(get_package_manager_install_cmd "$distro")
        local packages=$(get_dependencies_for_os "$distro" true true)
        
        echo -e "${BLUE}$distro${NC}:"
        echo "$install_cmd $packages"
        echo ""
    done
    
    log "SUCCESS" "Dependency synchronization complete"
}

# Main function
main() {
    echo -e "${BLUE}"
    echo "=========================================="
    echo "  OneFileLinux Dependency Synchronizer"
    echo "=========================================="
    echo -e "${NC}"
    
    # Check required files
    check_files
    
    # Perform dependency synchronization
    sync_dependencies
    
    if [ "$DRY_RUN" = true ]; then
        log "INFO" "Dry run completed. No changes were made."
    else
        log "SUCCESS" "Dependency synchronization completed successfully."
    fi
}

# Execute main function
main