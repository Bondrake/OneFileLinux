#!/bin/bash
#
# OneFileLinux build environment preparation script
# Detects OS and installs required dependencies
#

# Define script name for error handling
SCRIPT_NAME=$(basename "$0")

# Source the core library first (required)
if [ ! -f "./80_common.sh" ]; then
    echo "ERROR: Critical library file not found: ./80_common.sh"
    exit 1
fi
source ./80_common.sh

# Source all library scripts using the source_libraries function
source_libraries "."

# Function to check if we're running as root/sudo
check_root() {
    if [ "$EUID" -ne 0 ]; then
        log "ERROR" "This script requires root/sudo privileges to install packages."
        log "INFO" "Please run as root or with sudo."
        exit 1
    fi
}

# Check for required build tools
check_required_tools() {
    local missing_tools=()
    
    # These tools should be available regardless of OS
    for tool in make gcc g++ ld; do
        if ! command -v $tool &> /dev/null; then
            missing_tools+=("$tool")
        fi
    done
    
    if [ ${#missing_tools[@]} -ne 0 ]; then
        log "WARNING" "The following essential build tools are missing:"
        printf "  - %s\n" "${missing_tools[@]}"
        log "INFO" "These will be installed by the script."
    fi
}

# Detect available memory and disk space
check_system_resources() {
    log "INFO" "Checking system resources..."
    
    # Check available memory
    if [ -f /proc/meminfo ]; then
        local mem_total=$(grep MemTotal /proc/meminfo | awk '{print $2}')
        local mem_gb=$(awk "BEGIN {printf \"%.1f\", $mem_total/1024/1024}")
        
        log "INFO" "Available memory: ${mem_gb}GB"
        if (( $(echo "$mem_gb < 2.0" | bc -l) )); then
            log "WARNING" "Low memory detected. Build might be slow or fail."
            log "INFO" "Recommended: At least 2GB of RAM"
        fi
    fi
    
    # Check available disk space
    local build_dir=$(pwd)
    local available_space=$(df -BG $build_dir | awk 'NR==2 {print $4}' | sed 's/G//')
    
    log "INFO" "Available disk space: ${available_space}GB"
    if (( $(echo "$available_space < 10" | bc -l) )); then
        log "WARNING" "Low disk space. At least 10GB recommended."
        log "INFO" "The build process requires approximately 5GB, but more is recommended."
    fi
}

# Detect operation system
detect_os() {
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        OS=$NAME
        VER=$VERSION_ID
    elif [ -f /etc/debian_version ]; then
        OS="Debian"
        VER=$(cat /etc/debian_version)
    elif [ -f /etc/redhat-release ]; then
        OS=$(cat /etc/redhat-release | cut -d ' ' -f 1)
        VER=$(cat /etc/redhat-release | grep -oE '[0-9]+\.[0-9]+')
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        OS="macOS"
        VER=$(sw_vers -productVersion)
    else
        OS="Unknown"
        VER="Unknown"
    fi

    log "INFO" "Detected OS: $OS $VER"
}

# Prepare for build by creating required directories
prepare_directories() {
    log "INFO" "Preparing build directories..."
    
    # Create directories if they don't exist
    mkdir -p alpine-minirootfs/lib/modules
    mkdir -p alpine-minirootfs/dev
    
    # Create cache directory if caching is enabled
    if [ "${USE_CACHE:-true}" = "true" ]; then
        mkdir -p "${CACHE_DIR:-$HOME/.onefilelinux/cache}/sources"
        mkdir -p "${CACHE_DIR:-$HOME/.onefilelinux/cache}/ccache"
        mkdir -p "${CACHE_DIR:-$HOME/.onefilelinux/cache}/packages"
        log "INFO" "Cache directories prepared: ${CACHE_DIR:-$HOME/.onefilelinux/cache}"
    fi
    
    log "SUCCESS" "Directory structure prepared."
}

# Verify that essential build scripts exist
verify_scripts() {
    log "INFO" "Verifying build scripts..."
    
    local missing_scripts=()
    for script in 01_get.sh 02_chrootandinstall.sh 03_conf.sh 04_build.sh 99_cleanup.sh; do
        if [ ! -f "$script" ]; then
            missing_scripts+=("$script")
        elif [ ! -x "$script" ]; then
            log "INFO" "Making $script executable"
            chmod +x "$script"
        fi
    done
    
    if [ ${#missing_scripts[@]} -ne 0 ]; then
        log "ERROR" "The following required build scripts are missing:"
        printf "  - %s\n" "${missing_scripts[@]}"
        log "INFO" "Please ensure all build scripts are present in the FoxBuild directory."
        exit 1
    fi
    
    log "SUCCESS" "All build scripts are present and executable."
}

# Main function
main() {
    # Initialize script with standard header (prints banner)
    initialize_script
    
    detect_os
    check_required_tools
    check_system_resources
    
    log "INFO" ""
    # Install dependencies based on OS
    case "$OS" in
        "Ubuntu"|"Debian"|"Pop!_OS"|"Linux Mint"|"Fedora"|"CentOS"|"Red Hat Enterprise Linux"|"RHEL"|"Arch Linux"|"Manjaro Linux"|"Alpine Linux")
            check_root
            log "INFO" "Installing dependencies for $OS..."
            
            # Use dependency helper to determine the correct install command and packages
            if [ -f "85_dependency_helper.sh" ]; then
                # Source the dependency helper if not already loaded
                if ! is_library_loaded "85_dependency_helper"; then
                    source_library "./85_dependency_helper.sh"
                fi
                
                # Get the install command and execute it
                local install_cmd=$(get_os_install_command)
                log "INFO" "Running: $install_cmd"
                eval "$install_cmd"
                
                log "SUCCESS" "Installed all required dependencies for $OS"
            else
                # Fallback if dependency helper is not available
                log "WARNING" "Dependency helper not found, using hardcoded dependencies"
                case "$OS" in
                    "Ubuntu"|"Debian"|"Pop!_OS"|"Linux Mint")
                        apt-get update
                        apt-get install -y wget tar xz-utils build-essential flex bison libssl-dev bc kmod libelf-dev xz-utils lzma zstd upx-ucl ccache
                        ;;
                    "Fedora"|"CentOS"|"Red Hat Enterprise Linux"|"RHEL")
                        if command -v dnf &> /dev/null; then
                            dnf install -y wget tar xz gcc make flex bison openssl-devel bc kmod elfutils-libelf-devel xz zstd upx ccache
                        else
                            yum install -y wget tar xz gcc make flex bison openssl-devel bc kmod elfutils-libelf-devel xz zstd upx ccache
                        fi
                        ;;
                    "Arch Linux"|"Manjaro Linux")
                        pacman -Sy --noconfirm wget tar xz base-devel flex bison openssl bc kmod libelf xz zstd upx ccache
                        ;;
                    "Alpine Linux")
                        apk add wget tar xz build-base flex bison openssl-dev bc kmod libelf elfutils-dev xz zstd upx ccache
                        ;;
                esac
            fi
            ;;
        "macOS")
            log "WARNING" "macOS detected. You need a Linux environment to build OneFileLinux."
            log "INFO" "We recommend using Docker or a Linux VM. See the README for details."
            log "INFO" ""
            log "INFO" "For Docker, you can use:"
            log "INFO" "  docker run -it --rm -v $(pwd)/..:/build ubuntu:latest bash"
            log "INFO" "  cd /build/build && ./00_prepare.sh"
            log "INFO" ""
            log "INFO" "If you have homebrew installed, we can install wget which is needed for downloading:"
            if command -v brew &> /dev/null; then
                read -p "Install wget using Homebrew? [y/N] " -n 1 -r
                echo
                if [[ $REPLY =~ ^[Yy]$ ]]; then
                    brew install wget
                fi
            else
                log "INFO" "Homebrew not detected. Please install wget manually if needed."
            fi
            exit 0
            ;;
        *)
            log "ERROR" "Unsupported or unknown OS: $OS"
            log "INFO" "Please install the following packages manually:"
            log "INFO" "- wget, tar, xz-utils"
            log "INFO" "- gcc, make, build tools"
            log "INFO" "- flex, bison"
            log "INFO" "- libssl-dev/openssl-devel"
            log "INFO" "- bc, kmod, libelf-dev"
            exit 1
            ;;
    esac

    # Final preparations
    verify_scripts
    prepare_directories

    log "INFO" ""
    log "SUCCESS" "Environment preparation complete!"
    log "INFO" "You can now run the build scripts in sequence:"
    log "INFO" "  ./01_get.sh"
    log "INFO" "  ./02_chrootandinstall.sh"
    log "INFO" "  ./03_conf.sh"
    log "INFO" "  ./04_build.sh"
    log "INFO" ""
    log "INFO" "After a successful build, you can clean up with:"
    log "INFO" "  ./99_cleanup.sh"
}

# Execute main function
main