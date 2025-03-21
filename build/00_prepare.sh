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
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS memory check (using sysctl)
        local mem_total=$(sysctl -n hw.memsize 2>/dev/null)
        if [ -n "$mem_total" ]; then
            local mem_gb=$(awk "BEGIN {printf \"%.1f\", $mem_total/1024/1024/1024}")
            log "INFO" "Available memory: ${mem_gb}GB"
            if (( $(echo "$mem_gb < 2.0" | bc -l) )); then
                log "WARNING" "Low memory detected. Build might be slow or fail."
                log "INFO" "Recommended: At least 2GB of RAM"
            fi
        else
            log "WARNING" "Could not detect available memory on macOS"
        fi
    elif [ -f /proc/meminfo ]; then
        # Linux memory check
        local mem_total=$(grep MemTotal /proc/meminfo | awk '{print $2}')
        local mem_gb=$(awk "BEGIN {printf \"%.1f\", $mem_total/1024/1024}")
        
        log "INFO" "Available memory: ${mem_gb}GB"
        if (( $(echo "$mem_gb < 2.0" | bc -l) )); then
            log "WARNING" "Low memory detected. Build might be slow or fail."
            log "INFO" "Recommended: At least 2GB of RAM"
        fi
    else
        log "WARNING" "Could not detect available memory"
    fi
    
    # Check available disk space
    local build_dir=$(pwd)
    
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS disk space check
        local available_space=$(df -h $build_dir | awk 'NR==2 {print $4}' | sed 's/Gi//')
    else
        # Linux disk space check
        local available_space=$(df -BG $build_dir | awk 'NR==2 {print $4}' | sed 's/G//')
    fi
    
    log "INFO" "Available disk space: ${available_space}GB"
    if [ -n "$available_space" ] && (( $(echo "$available_space < 10" | bc -l) )); then
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
            log "WARNING" "⚠️  Building on macOS is EXPERIMENTAL and not fully supported ⚠️"
            log "WARNING" "Full kernel builds will fail on macOS without a cross-compiler"
            log "INFO" "For production builds, please use Docker: cd $(pwd)/../docker && ./build-onefilelinux.sh"
            log "INFO" ""
            
            # Check for Homebrew
            if ! command -v brew &> /dev/null; then
                log "WARNING" "Homebrew not detected. This is strongly recommended for macOS builds."
                log "INFO" "Install Homebrew with: /bin/bash -c \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""
                read -p "Do you want to continue without Homebrew? [y/N] " -n 1 -r
                echo
                if [[ ! $REPLY =~ ^[Yy]$ ]]; then
                    log "INFO" "Exiting. Please install Homebrew and run this script again."
                    exit 0
                fi
            else
                # Homebrew is installed, check/install required packages
                log "SUCCESS" "Homebrew detected: $(brew --version | head -1 | awk '{print $2}')"
                
                # Check Bash version
                BASH_VERSION=$(bash --version | head -1 | awk '{print $4}' | cut -d'.' -f1)
                if [[ $BASH_VERSION -lt 4 ]]; then
                    log "WARNING" "Bash version less than 4.0 detected: $BASH_VERSION"
                    log "INFO" "Newer Bash is required for associative arrays."
                    read -p "Install Bash 4+ with Homebrew? [Y/n] " -n 1 -r
                    echo
                    if [[ ! $REPLY =~ ^[Nn]$ ]]; then
                        brew install bash
                        log "SUCCESS" "Bash installed. Please use /usr/local/bin/bash to run the build scripts."
                        log "INFO" "Example: /usr/local/bin/bash build.sh"
                    else
                        log "WARNING" "Continuing without Bash 4+. The build may fail."
                    fi
                else
                    log "SUCCESS" "Bash version OK: $BASH_VERSION+"
                fi
                
                # Check for GNU Make 4.0+
                MAKE_INSTALLED=false
                if command -v gmake &> /dev/null; then
                    GMAKE_VERSION=$(gmake --version | head -1 | awk '{print $3}')
                    log "SUCCESS" "GNU Make detected: $GMAKE_VERSION"
                    MAKE_INSTALLED=true
                else
                    log "WARNING" "GNU Make 4.0+ not found. This is required for kernel compilation."
                    read -p "Install GNU Make with Homebrew? [Y/n] " -n 1 -r
                    echo
                    if [[ ! $REPLY =~ ^[Nn]$ ]]; then
                        brew install make
                        log "SUCCESS" "GNU Make installed as 'gmake'"
                        MAKE_INSTALLED=true
                    else
                        log "WARNING" "Continuing without GNU Make 4.0+. Kernel build will fail."
                    fi
                fi
                
                # Check for wget (needed for downloads)
                if ! command -v wget &> /dev/null; then
                    log "WARNING" "wget not found. This is needed for downloading sources."
                    read -p "Install wget with Homebrew? [Y/n] " -n 1 -r
                    echo
                    if [[ ! $REPLY =~ ^[Nn]$ ]]; then
                        brew install wget
                        log "SUCCESS" "wget installed"
                    else
                        log "WARNING" "Continuing without wget. Downloads may fail."
                    fi
                else
                    log "SUCCESS" "wget detected: $(wget --version | head -1 | awk '{print $3}')"
                fi
                
                # Install other helpful utilities
                log "INFO" "Checking for other helpful utilities..."
                MISSING_UTILS=()
                
                for util in coreutils findutils grep; do
                    if ! brew list --formula | grep -q "^$util\$"; then
                        MISSING_UTILS+=($util)
                    fi
                done
                
                if [ ${#MISSING_UTILS[@]} -gt 0 ]; then
                    log "INFO" "Some helpful GNU utilities are missing: ${MISSING_UTILS[*]}"
                    read -p "Install these utilities with Homebrew? [Y/n] " -n 1 -r
                    echo
                    if [[ ! $REPLY =~ ^[Nn]$ ]]; then
                        brew install ${MISSING_UTILS[*]}
                        log "SUCCESS" "GNU utilities installed"
                    else
                        log "WARNING" "Missing GNU utilities may cause build issues."
                        log "INFO" "Install them later with: brew install ${MISSING_UTILS[*]}"
                    fi
                else
                    log "SUCCESS" "GNU findutils and grep detected"
                fi
                
                # Add GNU tools to path
                if brew --prefix findutils &>/dev/null || brew --prefix grep &>/dev/null; then
                    log "INFO" "Adding GNU tools to PATH with higher priority"
                    export PATH="$(brew --prefix)/opt/findutils/libexec/gnubin:$(brew --prefix)/opt/grep/libexec/gnubin:$PATH"
                fi
            fi
            
            # Provide important guidance for MacOS builds
            log "INFO" ""
            log "INFO" "For development/testing on macOS:"
            log "INFO" "  1. Use --dry-run to test configurations: ./build.sh build -- --minimal --dry-run"
            
            if [ "$MAKE_INSTALLED" = true ]; then
                log "INFO" "  2. You can use ./build.sh for parameter testing (gmake will be used automatically)"
            else
                log "INFO" "  2. Install GNU Make 4.0+: brew install make"
            fi
            
            log "INFO" "  3. For actual builds, use Docker as described above"
            log "INFO" ""
            
            # Ask if user wants to continue
            read -p "Continue with preparation? [Y/n] " -n 1 -r
            echo
            if [[ $REPLY =~ ^[Nn]$ ]]; then
                log "INFO" "Exiting preparation. Consider using Docker for full builds."
                exit 0
            fi
            
            # Continue with the script
            log "INFO" "Continuing with macOS preparation..."
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