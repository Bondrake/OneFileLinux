#!/bin/bash
#
# OneFileLinux unified build script
# Runs the entire build process with a single command
#

# Default settings
CLEAN_START=false
VERBOSE=false
SKIP_PREPARE=false
CLEAN_END=false
BUILD_STEP=""

# Default Build Profile (will be loaded from profile definitions)
BUILD_PROFILE="standard"

# Optional components with default values
# These will be overridden by the selected build profile
INCLUDE_ZFS=true
INCLUDE_BTRFS=false
INCLUDE_RECOVERY_TOOLS=true
INCLUDE_NETWORK_TOOLS=true
INCLUDE_CRYPTO=true
INCLUDE_TUI=true
INCLUDE_MINIMAL_KERNEL=false
INCLUDE_COMPRESSION=true
COMPRESSION_TOOL="upx"  # Options: upx, xz, zstd

# Advanced package groups
INCLUDE_ADVANCED_FS=false      # Advanced filesystem tools
INCLUDE_DISK_DIAG=false        # Disk and hardware diagnostics
INCLUDE_NETWORK_DIAG=false     # Network diagnostics and VPN
INCLUDE_SYSTEM_TOOLS=false     # Advanced system utilities
INCLUDE_DATA_RECOVERY=false    # Advanced data recovery tools
INCLUDE_BOOT_REPAIR=false      # Boot repair utilities
INCLUDE_EDITORS=false          # Advanced text editors
INCLUDE_SECURITY=false         # Security analysis tools

# Build performance options
USE_CACHE=true
CACHE_DIR="${HOME}/.onefilelinux/cache"
BUILD_JOBS=$(getconf _NPROCESSORS_ONLN)
KEEP_CCACHE=true
USE_SWAP=false        # Create swap file if memory is low
INTERACTIVE_CONFIG=false  # Use interactive kernel configuration

# Kernel configuration options
AUTO_KERNEL_CONFIG=true         # Automatically apply feature-specific options

# Customization options
CUSTOM_KERNEL_CONFIG=""  # Path to custom kernel config
EXTRA_PACKAGES=""        # Comma-separated list of additional packages

# Security options
ROOT_PASSWORD=""
GENERATE_RANDOM_PASSWORD=true
ROOT_PASSWORD_LENGTH=12

# Define config file location
CONFIG_FILE="./build.conf"

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

# Set a default build profile early 
if type -t apply_build_profile &>/dev/null; then
    apply_build_profile "$BUILD_PROFILE"
fi

# Set error handling
trap 'echo -e "${RED}[ERROR]${NC} An error occurred at line $LINENO. Command: $BASH_COMMAND"; exit 1' ERR
set -e

# Define paths for use by build core
BUILD_DIR="$(pwd)"
ROOTFS_DIR="$BUILD_DIR/alpine-minirootfs"
KERNEL_DIR="$BUILD_DIR/linux"
ZFS_DIR="$BUILD_DIR/zfs"
ZFILES_DIR="$BUILD_DIR/zfiles"
OUTPUT_DIR="$BUILD_DIR/../output"

# Export paths for use by the build_core library functions
export BUILD_DIR ROOTFS_DIR KERNEL_DIR ZFS_DIR ZFILES_DIR OUTPUT_DIR

# Display usage information
usage() {
    echo "Usage: $0 [options] [STEP] [-- <arguments for 04_build.sh>]"
    echo ""
    echo "Options:"
    echo "  -h, --help          Display this help message"
    echo "  -c, --clean-start   Run cleanup script before starting (removes previous builds)"
    echo "  -v, --verbose       Enable verbose output"
    echo "  -s, --skip-prepare  Skip environment preparation step"
    echo "  -C, --clean-end     Run cleanup script after successful build"
    echo ""
    echo "Steps:"
    echo "  all                 Run all build steps (default)"
    echo "  prepare             Run only environment preparation (00_prepare.sh)"
    echo "  get                 Run through downloading sources (01_get.sh)"
    echo "  chroot              Run through chroot and install (02_chrootandinstall.sh)"
    echo "  conf                Run through configuration (03_conf.sh)"
    echo "  build               Run only the build step (04_build.sh)"
    echo "  clean               Run only cleanup (99_cleanup.sh)"
    echo ""
    echo "Passthrough Arguments:"
    echo "  You can pass arguments directly to 04_build.sh by using a double dash (--)"
    echo "  followed by the arguments. These arguments will be passed as-is to 04_build.sh."
    echo "  Example: $0 build -- --minimal --without-zfs"
    echo ""
    echo "Examples:"
    echo "  $0                  Run all build steps"
    echo "  $0 -c all           Clean, then run all build steps"
    echo "  $0 get              Run only the download step"
    echo "  $0 all -- --minimal --without-zfs     Run all steps and pass arguments to 04_build.sh"
    echo "  $0 build -- --minimal --without-zfs   Run only build step with custom arguments"
    echo "  $0 -s build         Skip environment preparation and run only the build step"
    echo ""
}

# Load configuration from file
load_config() {
    if [ -f "$CONFIG_FILE" ]; then
        log "INFO" "Loading configuration from $CONFIG_FILE"
        source "$CONFIG_FILE"
        log "SUCCESS" "Configuration loaded successfully"
    else
        log "INFO" "No configuration file found, using defaults"
    fi
}

# Save configuration to file
save_config() {
    log "INFO" "Saving configuration to $CONFIG_FILE"
    cat > "$CONFIG_FILE" << EOF
# OneFileLinux build configuration
# Generated by build.sh on $(date)

# Optional components
INCLUDE_ZFS=$INCLUDE_ZFS
INCLUDE_BTRFS=$INCLUDE_BTRFS
INCLUDE_RECOVERY_TOOLS=$INCLUDE_RECOVERY_TOOLS
INCLUDE_NETWORK_TOOLS=$INCLUDE_NETWORK_TOOLS
INCLUDE_CRYPTO=$INCLUDE_CRYPTO
INCLUDE_TUI=$INCLUDE_TUI
INCLUDE_MINIMAL_KERNEL=$INCLUDE_MINIMAL_KERNEL
INCLUDE_COMPRESSION=$INCLUDE_COMPRESSION
COMPRESSION_TOOL="$COMPRESSION_TOOL"

# Build performance options
USE_CACHE=$USE_CACHE
CACHE_DIR="$CACHE_DIR"
BUILD_JOBS=$BUILD_JOBS
KEEP_CCACHE=$KEEP_CCACHE
USE_SWAP=$USE_SWAP
INTERACTIVE_CONFIG=$INTERACTIVE_CONFIG
AUTO_KERNEL_CONFIG=$AUTO_KERNEL_CONFIG

# Customization options
CUSTOM_KERNEL_CONFIG="$CUSTOM_KERNEL_CONFIG"
EXTRA_PACKAGES="$EXTRA_PACKAGES"

# Advanced package groups
INCLUDE_ADVANCED_FS=$INCLUDE_ADVANCED_FS
INCLUDE_DISK_DIAG=$INCLUDE_DISK_DIAG
INCLUDE_NETWORK_DIAG=$INCLUDE_NETWORK_DIAG
INCLUDE_SYSTEM_TOOLS=$INCLUDE_SYSTEM_TOOLS
INCLUDE_DATA_RECOVERY=$INCLUDE_DATA_RECOVERY
INCLUDE_BOOT_REPAIR=$INCLUDE_BOOT_REPAIR
INCLUDE_EDITORS=$INCLUDE_EDITORS
INCLUDE_SECURITY=$INCLUDE_SECURITY

# Security options
GENERATE_RANDOM_PASSWORD=$GENERATE_RANDOM_PASSWORD
ROOT_PASSWORD_LENGTH=$ROOT_PASSWORD_LENGTH
EOF
    log "SUCCESS" "Configuration saved successfully"
}

# Print current configuration
print_config() {
    log "INFO" "Current build configuration:"
    
    # Determine build profile if the function exists
    local profile_name=""
    if type -t get_build_profile_name &>/dev/null; then
        profile_name=$(get_build_profile_name)
        log "INFO" "  Build profile: ${GREEN}${profile_name}${NC}"
    else
        # Fallback to basic detection
        if [ "$INCLUDE_MINIMAL_KERNEL" = "true" ]; then
            log "INFO" "  Build type: ${YELLOW}Minimal${NC} (optimized for size)"
        else
            log "INFO" "  Build type: ${GREEN}Standard${NC}"
        fi
    fi
    
    # Show EFI output filename
    if [ -n "$profile_name" ]; then
        log "INFO" "  Output file: OneFileLinux-${profile_name}.efi"
    fi
    
    log "INFO" "  ZFS support: $(bool_to_str $INCLUDE_ZFS)"
    log "INFO" "  Btrfs support: $(bool_to_str $INCLUDE_BTRFS)"
    log "INFO" "  Recovery tools: $(bool_to_str $INCLUDE_RECOVERY_TOOLS)"
    log "INFO" "  Network tools: $(bool_to_str $INCLUDE_NETWORK_TOOLS)"
    log "INFO" "  Crypto support: $(bool_to_str $INCLUDE_CRYPTO)"
    log "INFO" "  Text User Interface: $(bool_to_str $INCLUDE_TUI)"
    
    # Display compression information
    if [ "$INCLUDE_COMPRESSION" = "true" ]; then
        log "INFO" "  EFI Compression: ${GREEN}Yes${NC} (using ${COMPRESSION_TOOL})"
    else
        log "INFO" "  EFI Compression: ${RED}No${NC}"
    fi
    
    # Display build performance settings
    log "INFO" ""
    log "INFO" "Build performance settings:"
    log "INFO" "  Source caching: $(bool_to_str $USE_CACHE)"
    if [ "$USE_CACHE" = "true" ]; then
        log "INFO" "  Cache directory: $CACHE_DIR"
    fi
    log "INFO" "  Parallel jobs: ${GREEN}$BUILD_JOBS${NC}"
    log "INFO" "  Keep ccache: $(bool_to_str $KEEP_CCACHE)"
    log "INFO" "  Use swap file: $(bool_to_str $USE_SWAP)"
    log "INFO" "  Interactive kernel config: $(bool_to_str $INTERACTIVE_CONFIG)"
    
    # Display customization settings
    log "INFO" ""
    log "INFO" "Customization settings:"
    if [ -n "$CUSTOM_KERNEL_CONFIG" ]; then
        log "INFO" "  Custom kernel config: ${GREEN}$CUSTOM_KERNEL_CONFIG${NC}"
    else
        log "INFO" "  Kernel config: ${BLUE}Default${NC}"
    fi
    if [ -n "$EXTRA_PACKAGES" ]; then
        log "INFO" "  Extra packages: ${GREEN}$EXTRA_PACKAGES${NC}"
    else
        log "INFO" "  Extra packages: ${BLUE}None${NC}"
    fi
    
    # Display advanced package groups
    log "INFO" ""
    log "INFO" "Advanced package groups:"
    log "INFO" "  Advanced filesystem tools: $(bool_to_str $INCLUDE_ADVANCED_FS)"
    log "INFO" "  Disk & hardware diagnostics: $(bool_to_str $INCLUDE_DISK_DIAG)"
    log "INFO" "  Network diagnostics & VPN: $(bool_to_str $INCLUDE_NETWORK_DIAG)"
    log "INFO" "  Advanced system tools: $(bool_to_str $INCLUDE_SYSTEM_TOOLS)"
    log "INFO" "  Data recovery utilities: $(bool_to_str $INCLUDE_DATA_RECOVERY)"
    log "INFO" "  Boot repair tools: $(bool_to_str $INCLUDE_BOOT_REPAIR)"
    log "INFO" "  Advanced text editors: $(bool_to_str $INCLUDE_EDITORS)"
    log "INFO" "  Security tools: $(bool_to_str $INCLUDE_SECURITY)"
    
    # Display security settings
    log "INFO" ""
    log "INFO" "Security settings:"
    if [ "$GENERATE_RANDOM_PASSWORD" = "true" ]; then
        log "INFO" "  Root password: ${GREEN}Generate random password${NC} (length: $ROOT_PASSWORD_LENGTH)"
    elif [ -n "$ROOT_PASSWORD" ]; then
        log "INFO" "  Root password: ${GREEN}Custom password provided${NC}"
    else
        log "INFO" "  Root password: ${RED}None${NC} (unsafe)"
    fi
}

# bool_to_str is now provided by 84_build_core.sh

# Print extended usage information
usage_modules() {
    echo ""
    echo "Build Profile Options:"
    echo "  --minimal              Build with minimal profile (optimized for size, ~30-50% smaller)"
    echo "  --standard             Build with standard profile (default)"
    echo "  --full                 Build with full profile (all components)"
    echo "  --profile=NAME         Specify a custom profile by name"
    echo "  --list-profiles        List all available build profiles"
    echo ""
    echo "Size Optimization Options:"
    echo "  --with-compression     Enable EFI file compression (default: yes)"
    echo "  --without-compression  Disable EFI file compression (faster boot)"
    echo "  --compression-tool=TOOL Select compression tool (upx, xz, zstd) (default: upx)"
    echo ""
    echo "Build Performance Options:"
    echo "  --use-cache            Enable source and build caching (default: yes)"
    echo "  --no-cache             Disable source and build caching"
    echo "  --cache-dir=DIR        Set cache directory (default: ~/.onefilelinux/cache)"
    echo "  --jobs=N               Set number of parallel build jobs (default: CPU cores)"
    echo "  --keep-ccache          Keep compiler cache between builds (default: yes)"
    echo "  --no-keep-ccache       Clear compiler cache between builds"
    echo "  --use-swap             Create swap file if memory is low (default: no)"
    echo "  --no-swap              Do not create swap file even if memory is low"
    echo "  --interactive-config   Use interactive kernel configuration (menuconfig)"
    echo "  --no-interactive-config Use non-interactive kernel config (default)"
    echo "  --auto-kernel-config      Automatically apply feature-specific kernel options (default: yes)"
    echo "  --no-auto-kernel-config   Don't automatically apply feature-specific kernel options"
    echo ""
    echo "Security Options:"
    echo "  --password=PASS        Set custom root password (CAUTION: visible in process list)"
    echo "  --random-password      Generate random root password (default)"
    echo "  --no-password          Create root account with no password (unsafe)"
    echo "  --password-length=N    Set length of random password (default: 12)"
    echo ""
    echo "Optional Modules (Override Profile Settings):"
    echo "  --with-zfs             Include ZFS filesystem support (default in standard: yes)"
    echo "  --without-zfs          Exclude ZFS filesystem support"
    echo "  --with-btrfs           Include Btrfs filesystem support (default in standard: no)"
    echo "  --without-btrfs        Exclude Btrfs filesystem support"
    echo "  --with-recovery-tools  Include data recovery tools (default in standard: yes)"
    echo "  --without-recovery-tools  Exclude data recovery tools"
    echo "  --with-network-tools   Include network tools (default in standard: yes)"
    echo "  --without-network-tools  Exclude network tools"
    echo "  --with-crypto          Include encryption support (default in standard: yes)"
    echo "  --without-crypto       Exclude encryption support"
    echo "  --with-tui             Include Text User Interface (default in standard: yes)"
    echo "  --without-tui          Exclude Text User Interface"
    echo ""
    echo "Configuration Management:"
    echo "  --save-config          Save current configuration as default"
    echo "  --show-config          Display current build configuration"
    echo ""
    echo "Examples:"
    echo "  $0 --profile=standard   Build with standard profile"
    echo "  $0 --list-profiles      List all available build profiles"
    echo "  $0 --minimal            Build with minimal profile (no ZFS, minimal kernel)"
    echo "  $0 --full               Build with full profile (all components)"
    echo "  $0 --profile=standard --with-btrfs  Use standard profile but add Btrfs support"
    echo "  $0 --profile=standard --without-zfs  Use standard profile but exclude ZFS"
    echo "  $0 --compression-tool=zstd  Use ZSTD for compression instead of UPX"
    echo "  $0 --minimal --compression-tool=xz  Minimal build with highest compression"
    echo "  $0 --without-compression  Disable compression for faster boot time"
    echo "  $0 --jobs=8             Use 8 parallel build jobs"
    echo "  $0 --use-swap           Create swap file if system has low memory"
    echo "  $0 --password=mypassword  Set a specific root password"
    echo "  $0 --random-password    Generate a secure random root password"
    echo "  $0 --password-length=16  Set random password length to 16 characters"
    echo ""
}

# Process command line arguments
process_args() {
    # First load config file if it exists
    load_config
    
    # Check for -- separator which indicates passthrough arguments for 04_build.sh
    local found_separator=false
    local separator_index=-1
    local all_args=("$@")
    
    for ((i=0; i<$#; i++)); do
        if [ "${all_args[$i]}" = "--" ]; then
            found_separator=true
            separator_index=$i
            break
        fi
    done
    
    # If we found a separator, extract passthrough arguments
    if [ "$found_separator" = true ]; then
        # Save the passthrough arguments for later use
        local passthrough_args=("${all_args[@]:$((separator_index+1))}")
        BUILD_PASSTHROUGH_ARGS="${passthrough_args[*]}"
        
        # Remove the separator and passthrough arguments from the argument list
        set -- "${all_args[@]:0:$separator_index}"
        
        log "INFO" "Passthrough arguments for 04_build.sh: $BUILD_PASSTHROUGH_ARGS"
    fi
    
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                usage
                usage_modules
                exit 0
                ;;
            -c|--clean-start)
                CLEAN_START=true
                shift
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -s|--skip-prepare)
                SKIP_PREPARE=true
                shift
                ;;
            -C|--clean-end)
                CLEAN_END=true
                shift
                ;;
            --profile=*)
                BUILD_PROFILE="${1#*=}"
                # Apply the profile using the build_profiles module
                apply_build_profile "$BUILD_PROFILE" || {
                    log "ERROR" "Invalid build profile: $BUILD_PROFILE"
                    log "INFO" "Available profiles:"
                    list_build_profiles
                    exit 1
                }
                log "SUCCESS" "Applied build profile: $BUILD_PROFILE"
                shift
                ;;
            --list-profiles)
                list_build_profiles
                exit 0
                ;;
            --with-zfs)
                INCLUDE_ZFS=true
                shift
                ;;
            --without-zfs)
                INCLUDE_ZFS=false
                shift
                ;;
            --with-btrfs)
                INCLUDE_BTRFS=true
                shift
                ;;
            --without-btrfs)
                INCLUDE_BTRFS=false
                shift
                ;;
            --with-recovery-tools)
                INCLUDE_RECOVERY_TOOLS=true
                shift
                ;;
            --without-recovery-tools)
                INCLUDE_RECOVERY_TOOLS=false
                shift
                ;;
            --with-network-tools)
                INCLUDE_NETWORK_TOOLS=true
                shift
                ;;
            --without-network-tools)
                INCLUDE_NETWORK_TOOLS=false
                shift
                ;;
            --with-crypto)
                INCLUDE_CRYPTO=true
                shift
                ;;
            --without-crypto)
                INCLUDE_CRYPTO=false
                shift
                ;;
            --with-tui)
                INCLUDE_TUI=true
                shift
                ;;
            --without-tui)
                INCLUDE_TUI=false
                shift
                ;;
            --with-compression)
                INCLUDE_COMPRESSION=true
                shift
                ;;
            --without-compression)
                INCLUDE_COMPRESSION=false
                shift
                ;;
            # Advanced package groups
            --with-advanced-fs)
                INCLUDE_ADVANCED_FS=true
                shift
                ;;
            --without-advanced-fs)
                INCLUDE_ADVANCED_FS=false
                shift
                ;;
            --with-disk-diag)
                INCLUDE_DISK_DIAG=true
                shift
                ;;
            --without-disk-diag)
                INCLUDE_DISK_DIAG=false
                shift
                ;;
            --with-network-diag)
                INCLUDE_NETWORK_DIAG=true
                shift
                ;;
            --without-network-diag)
                INCLUDE_NETWORK_DIAG=false
                shift
                ;;
            --with-system-tools)
                INCLUDE_SYSTEM_TOOLS=true
                shift
                ;;
            --without-system-tools)
                INCLUDE_SYSTEM_TOOLS=false
                shift
                ;;
            --with-data-recovery)
                INCLUDE_DATA_RECOVERY=true
                shift
                ;;
            --without-data-recovery)
                INCLUDE_DATA_RECOVERY=false
                shift
                ;;
            --with-boot-repair)
                INCLUDE_BOOT_REPAIR=true
                shift
                ;;
            --without-boot-repair)
                INCLUDE_BOOT_REPAIR=false
                shift
                ;;
            --with-editors)
                INCLUDE_EDITORS=true
                shift
                ;;
            --without-editors)
                INCLUDE_EDITORS=false
                shift
                ;;
            --with-security)
                INCLUDE_SECURITY=true
                shift
                ;;
            --without-security)
                INCLUDE_SECURITY=false
                shift
                ;;
            --with-all-advanced)
                INCLUDE_ADVANCED_FS=true
                INCLUDE_DISK_DIAG=true
                INCLUDE_NETWORK_DIAG=true
                INCLUDE_SYSTEM_TOOLS=true
                INCLUDE_DATA_RECOVERY=true
                INCLUDE_BOOT_REPAIR=true
                INCLUDE_EDITORS=true
                INCLUDE_SECURITY=true
                log "INFO" "Enabling all advanced package groups"
                shift
                ;;
            --without-all-advanced)
                INCLUDE_ADVANCED_FS=false
                INCLUDE_DISK_DIAG=false
                INCLUDE_NETWORK_DIAG=false
                INCLUDE_SYSTEM_TOOLS=false
                INCLUDE_DATA_RECOVERY=false
                INCLUDE_BOOT_REPAIR=false
                INCLUDE_EDITORS=false
                INCLUDE_SECURITY=false
                log "INFO" "Disabling all advanced package groups"
                shift
                ;;
            --compression-tool=*)
                COMPRESSION_TOOL="${1#*=}"
                # Validate that the tool is one of the allowed options
                if [[ "$COMPRESSION_TOOL" != "upx" && "$COMPRESSION_TOOL" != "xz" && "$COMPRESSION_TOOL" != "zstd" ]]; then
                    log "ERROR" "Invalid compression tool: $COMPRESSION_TOOL. Allowed values: upx, xz, zstd"
                    exit 1
                fi
                shift
                ;;
            --use-cache)
                USE_CACHE=true
                shift
                ;;
            --no-cache)
                USE_CACHE=false
                shift
                ;;
            --cache-dir=*)
                CACHE_DIR="${1#*=}"
                shift
                ;;
            --jobs=*)
                BUILD_JOBS="${1#*=}"
                # Validate that the job count is a positive integer
                if ! [[ "$BUILD_JOBS" =~ ^[0-9]+$ ]] || [ "$BUILD_JOBS" -lt 1 ]; then
                    log "ERROR" "Invalid job count: $BUILD_JOBS. Must be a positive integer."
                    exit 1
                fi
                shift
                ;;
            --keep-ccache)
                KEEP_CCACHE=true
                shift
                ;;
            --no-keep-ccache)
                KEEP_CCACHE=false
                shift
                ;;
            --use-swap)
                USE_SWAP=true
                shift
                ;;
            --no-swap)
                USE_SWAP=false
                shift
                ;;
            --interactive-config)
                INTERACTIVE_CONFIG=true
                shift
                ;;
            --no-interactive-config)
                INTERACTIVE_CONFIG=false
                shift
                ;;
            --auto-kernel-config)
                AUTO_KERNEL_CONFIG=true
                shift
                ;;
            --no-auto-kernel-config)
                AUTO_KERNEL_CONFIG=false
                shift
                ;;
            --kernel-config=*)
                CUSTOM_KERNEL_CONFIG="${1#*=}"
                if [ ! -f "$CUSTOM_KERNEL_CONFIG" ]; then
                    log "ERROR" "Custom kernel configuration file not found: $CUSTOM_KERNEL_CONFIG"
                    exit 1
                fi
                log "INFO" "Using custom kernel configuration file: $CUSTOM_KERNEL_CONFIG"
                shift
                ;;
            --extra-packages=*)
                EXTRA_PACKAGES="${1#*=}"
                log "INFO" "Adding extra packages: $EXTRA_PACKAGES"
                shift
                ;;
            --password=*)
                ROOT_PASSWORD="${1#*=}"
                GENERATE_RANDOM_PASSWORD=false
                shift
                ;;
            --random-password)
                GENERATE_RANDOM_PASSWORD=true
                ROOT_PASSWORD=""
                shift
                ;;
            --no-password)
                GENERATE_RANDOM_PASSWORD=false
                ROOT_PASSWORD=""
                shift
                ;;
            --password-length=*)
                ROOT_PASSWORD_LENGTH="${1#*=}"
                # Validate that the password length is a reasonable number
                if ! [[ "$ROOT_PASSWORD_LENGTH" =~ ^[0-9]+$ ]] || [ "$ROOT_PASSWORD_LENGTH" -lt 8 ] || [ "$ROOT_PASSWORD_LENGTH" -gt 64 ]; then
                    log "ERROR" "Invalid password length: $ROOT_PASSWORD_LENGTH. Must be a number between 8 and 64."
                    exit 1
                fi
                shift
                ;;
            --minimal)
                apply_build_profile "minimal" || {
                    log "ERROR" "Failed to apply minimal profile"
                    exit 1
                }
                shift
                ;;
            --standard)
                apply_build_profile "standard" || {
                    log "ERROR" "Failed to apply standard profile"
                    exit 1
                }
                shift
                ;;
            --full)
                apply_build_profile "full" || {
                    log "ERROR" "Failed to apply full profile"
                    exit 1
                }
                shift
                ;;
            --save-config)
                save_config
                exit 0
                ;;
            --show-config)
                print_config
                exit 0
                ;;
            all|prepare|get|chroot|conf|build|clean)
                BUILD_STEP=$1
                shift
                ;;
            *)
                log "ERROR" "Unknown option: $1"
                usage
                exit 1
                ;;
        esac
    done
    
    # Default to "all" if no step is specified
    if [ -z "$BUILD_STEP" ]; then
        BUILD_STEP="all"
    fi
}

# Clean previous build
clean_build() {
    log "INFO" "Cleaning previous build..."
    if [ -f "./99_cleanup.sh" ]; then
        log "INFO" "Running cleanup script..."
        ./99_cleanup.sh
        log "SUCCESS" "Cleanup completed"
    else
        log "ERROR" "Cleanup script not found: ./99_cleanup.sh"
        exit 1
    fi
}

# Check required scripts
check_scripts() {
    log "INFO" "Checking required scripts..."
    local missing=()
    
    local scripts=("00_prepare.sh" "01_get.sh" "02_chrootandinstall.sh" "03_conf.sh" "04_build.sh" "99_cleanup.sh")
    
    for script in "${scripts[@]}"; do
        if [ ! -f "./$script" ]; then
            missing+=("$script")
        elif [ ! -x "./$script" ]; then
            log "INFO" "Making $script executable"
            chmod +x "./$script"
        fi
    done
    
    if [ ${#missing[@]} -ne 0 ]; then
        log "ERROR" "Missing required scripts:"
        for script in "${missing[@]}"; do
            log "ERROR" "  - $script"
        done
        exit 1
    fi
    
    log "SUCCESS" "All required scripts are present and executable"
}

# Save build progress
save_progress() {
    local step=$1
    echo "$step" > .build_progress
    log "INFO" "Build progress saved: $step"
}

# Load build progress
load_progress() {
    if [ -f .build_progress ]; then
        local step=$(cat .build_progress)
        log "INFO" "Found saved build progress: $step"
        echo "$step"
    else
        log "INFO" "No saved build progress found"
        echo ""
    fi
}

# Setup cache directory
setup_cache() {
    if [ "$USE_CACHE" = true ]; then
        log "INFO" "Setting up cache directory: $CACHE_DIR"
        
        # Create cache directories
        mkdir -p "$CACHE_DIR/sources"
        mkdir -p "$CACHE_DIR/ccache"
        mkdir -p "$CACHE_DIR/packages"
        mkdir -p "$CACHE_DIR/build"
        
        # Set up ccache if available
        if command -v ccache &> /dev/null; then
            export CCACHE_DIR="$CACHE_DIR/ccache"
            export PATH="/usr/lib/ccache:$PATH"
            log "SUCCESS" "Compiler cache enabled: $CCACHE_DIR"
            
            # Set ccache limits
            ccache -M 5G  # Set max cache size to 5GB
            ccache -z     # Zero statistics
        else
            log "WARNING" "ccache not found. Install ccache for faster rebuilds."
        fi
        
        log "SUCCESS" "Cache directories prepared"
    else
        log "INFO" "Caching disabled"
    fi
}

# Generate module configuration environment variables
generate_module_env() {
    local env_vars=""
    
    # Set environment variables for each module
    env_vars+="export INCLUDE_ZFS=$INCLUDE_ZFS "
    env_vars+="export INCLUDE_BTRFS=$INCLUDE_BTRFS "
    env_vars+="export INCLUDE_RECOVERY_TOOLS=$INCLUDE_RECOVERY_TOOLS "
    env_vars+="export INCLUDE_NETWORK_TOOLS=$INCLUDE_NETWORK_TOOLS "
    env_vars+="export INCLUDE_CRYPTO=$INCLUDE_CRYPTO "
    env_vars+="export INCLUDE_TUI=$INCLUDE_TUI "
    env_vars+="export INCLUDE_MINIMAL_KERNEL=$INCLUDE_MINIMAL_KERNEL "
    env_vars+="export INCLUDE_COMPRESSION=$INCLUDE_COMPRESSION "
    env_vars+="export COMPRESSION_TOOL=$COMPRESSION_TOOL "
    
    # Set build performance variables
    env_vars+="export USE_CACHE=$USE_CACHE "
    env_vars+="export CACHE_DIR=$CACHE_DIR "
    env_vars+="export BUILD_JOBS=$BUILD_JOBS "
    env_vars+="export KEEP_CCACHE=$KEEP_CCACHE "
    env_vars+="export USE_SWAP=$USE_SWAP "
    env_vars+="export INTERACTIVE_CONFIG=$INTERACTIVE_CONFIG "
    env_vars+="export AUTO_KERNEL_CONFIG=$AUTO_KERNEL_CONFIG "
    
    # Set customization variables
    env_vars+="export CUSTOM_KERNEL_CONFIG=\"$CUSTOM_KERNEL_CONFIG\" "
    env_vars+="export EXTRA_PACKAGES=\"$EXTRA_PACKAGES\" "
    
    # Set advanced package groups
    env_vars+="export INCLUDE_ADVANCED_FS=$INCLUDE_ADVANCED_FS "
    env_vars+="export INCLUDE_DISK_DIAG=$INCLUDE_DISK_DIAG "
    env_vars+="export INCLUDE_NETWORK_DIAG=$INCLUDE_NETWORK_DIAG "
    env_vars+="export INCLUDE_SYSTEM_TOOLS=$INCLUDE_SYSTEM_TOOLS "
    env_vars+="export INCLUDE_DATA_RECOVERY=$INCLUDE_DATA_RECOVERY "
    env_vars+="export INCLUDE_BOOT_REPAIR=$INCLUDE_BOOT_REPAIR "
    env_vars+="export INCLUDE_EDITORS=$INCLUDE_EDITORS "
    env_vars+="export INCLUDE_SECURITY=$INCLUDE_SECURITY "
    
    # Set security variables
    env_vars+="export ROOT_PASSWORD=\"$ROOT_PASSWORD\" "
    env_vars+="export GENERATE_RANDOM_PASSWORD=$GENERATE_RANDOM_PASSWORD "
    env_vars+="export ROOT_PASSWORD_LENGTH=$ROOT_PASSWORD_LENGTH "
    
    # Set ccache variables if enabled
    if [ "$USE_CACHE" = true ] && command -v ccache &> /dev/null; then
        env_vars+="export CCACHE_DIR=$CACHE_DIR/ccache "
        env_vars+="export PATH=/usr/lib/ccache:\$PATH "
    fi
    
    echo "$env_vars"
}

# Execute a build step
execute_step() {
    local step=$1
    local script=""
    local sudo_req=false
    
    # Shift to get any additional arguments after the step name
    shift
    local step_args=("$@")
    
    case $step in
        "prepare")
            script="./00_prepare.sh"
            sudo_req=true
            ;;
        "get")
            script="./01_get.sh"
            sudo_req=false
            ;;
        "chroot")
            script="./02_chrootandinstall.sh"
            sudo_req=true
            ;;
        "conf")
            script="./03_conf.sh"
            sudo_req=false
            ;;
        "build")
            script="./04_build.sh"
            sudo_req=false
            ;;
        "clean")
            script="./99_cleanup.sh"
            sudo_req=false
            ;;
        *)
            log "ERROR" "Unknown build step: $step"
            exit 1
            ;;
    esac
    
    # Check if script exists
    if [ ! -f "$script" ]; then
        log "ERROR" "Script not found: $script"
        exit 1
    fi
    
    # Add verbose flag if required
    if [ "$VERBOSE" = true ]; then
        export VERBOSE=true
    fi
    
    # Export module configuration variables
    eval $(generate_module_env)
    
    # Show module configuration if verbose
    if [ "$VERBOSE" = true ]; then
        print_config
    fi
    
    # Set FINALIZE_TIMING_LOG for the final build step when doing all steps
    if [ "$step" = "build" ] && [ "$BUILD_STEP" = "all" ]; then
        export FINALIZE_TIMING_LOG=true
        log "INFO" "Setting FINALIZE_TIMING_LOG=true for complete build timing summary"
    fi
    
    # Execute the script
    log "INFO" "Executing build step: $step"
    
    # For build step with direct arguments, pass them through without modification
    if [ "$step" = "build" ] && [ "${#step_args[@]}" -gt 0 ]; then
        log "INFO" "Running: $script ${step_args[*]}"
        
        if [ "$sudo_req" = true ] && [ "$EUID" -ne 0 ]; then
            log "INFO" "This step requires elevated privileges"
            sudo -E "$script" "${step_args[@]}"
        else
            "$script" "${step_args[@]}"
        fi
    else
        log "INFO" "Running: $script $args"
        
        if [ "$sudo_req" = true ] && [ "$EUID" -ne 0 ]; then
            log "INFO" "This step requires elevated privileges"
            sudo -E "$script" $args
        else
            "$script" $args
        fi
    fi
    
    local exit_code=$?
    if [ $exit_code -ne 0 ]; then
        log "ERROR" "Build step failed: $step (exit code: $exit_code)"
        exit $exit_code
    fi
    
    # Save progress
    save_progress "$step"
    log "SUCCESS" "Build step completed: $step"
}

# Main build function
run_build() {
    local start_step=""
    
    # Check if we have any passthrough arguments
    local passthrough_args=()
    if [ -n "$BUILD_PASSTHROUGH_ARGS" ]; then
        # Split the passthrough arguments by space
        read -ra passthrough_args <<< "$BUILD_PASSTHROUGH_ARGS"
    fi
    
    # If building "all", execute all steps in sequence
    if [ "$BUILD_STEP" = "all" ]; then
        # Determine where to start
        local steps=("prepare" "get" "chroot" "conf" "build")
        local start_idx=0
        
        if [ -n "$start_step" ]; then
            for i in "${!steps[@]}"; do
                if [ "${steps[$i]}" = "$start_step" ]; then
                    # Start from the next step
                    start_idx=$((i + 1))
                    break
                fi
            done
        fi
        
        # Execute all steps from the starting point
        if [ "$SKIP_PREPARE" = true ] && [ $start_idx -eq 0 ]; then
            start_idx=1
        fi
        
        for ((i=start_idx; i<${#steps[@]}; i++)); do
            # For the final build step, pass any passthrough arguments
            if [ "${steps[$i]}" = "build" ] && [ ${#passthrough_args[@]} -gt 0 ]; then
                execute_step "${steps[$i]}" "${passthrough_args[@]}"
            else
                execute_step "${steps[$i]}"
            fi
        done
    else
        # Execute only the specified step
        if [ "$BUILD_STEP" = "build" ] && [ ${#passthrough_args[@]} -gt 0 ]; then
            execute_step "$BUILD_STEP" "${passthrough_args[@]}"
        else
            execute_step "$BUILD_STEP"
        fi
    fi
}

# Main function
main() {
    # Initialize script with standard header (prints banner)
    initialize_script
    
    # Process command line arguments
    process_args "$@"
    
    # Display build configuration
    print_config
    
    # Setup cache if enabled
    if [ "$USE_CACHE" = true ]; then
        setup_cache
    fi
    
    # Check required scripts
    check_scripts
    
    # Clean build if requested
    if [ "$CLEAN_START" = true ]; then
        clean_build
    fi
    
    # Run the build
    run_build
    
    # Clean up after build if requested
    if [ "$CLEAN_END" = true ]; then
        clean_build
    fi
    
    # Print final message
    if [ "$BUILD_STEP" = "all" ]; then
        log "SUCCESS" "Build completed successfully!"
        if [ -f "../OneFileLinux.efi" ]; then
            local file_size=$(du -h "../OneFileLinux.efi" | cut -f1)
            log "SUCCESS" "Created OneFileLinux.efi (Size: $file_size)"
            
            # Show included features
            log "INFO" "Included features:"
            [ "$INCLUDE_ZFS" = true ] && log "INFO" "  - ZFS filesystem support"
            [ "$INCLUDE_BTRFS" = true ] && log "INFO" "  - Btrfs filesystem support"
            [ "$INCLUDE_RECOVERY_TOOLS" = true ] && log "INFO" "  - Data recovery tools"
            [ "$INCLUDE_NETWORK_TOOLS" = true ] && log "INFO" "  - Network tools"
            [ "$INCLUDE_CRYPTO" = true ] && log "INFO" "  - Encryption support"
            
            # Show password information
            log "INFO" ""
            if [ -f "onefilelinux-password.txt" ]; then
                PASSWORD_INFO=$(cat onefilelinux-password.txt)
                log "INFO" "Security information:"
                log "INFO" "  - ${GREEN}$PASSWORD_INFO${NC}"
                log "INFO" "  - Password file: $(pwd)/onefilelinux-password.txt"
            elif [ -n "$ROOT_PASSWORD" ]; then
                log "INFO" "Security information:"
                log "INFO" "  - ${GREEN}Custom root password set${NC}"
            else
                log "INFO" "Security information:"
                log "INFO" "  - ${RED}No root password set (unsafe)${NC}"
            fi
            
            # Show ccache stats if used
            if [ "$USE_CACHE" = true ] && command -v ccache &> /dev/null; then
                log "INFO" ""
                log "INFO" "Compiler cache statistics:"
                ccache -s | grep -E 'cache hit|cache miss|cache size' | while read line; do
                    log "INFO" "  $line"
                done
            fi
        fi
    else
        log "SUCCESS" "Build step '$BUILD_STEP' completed successfully!"
    fi
}

# Execute main with all command line arguments
main "$@"