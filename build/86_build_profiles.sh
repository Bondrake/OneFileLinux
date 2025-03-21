#!/usr/local/bin/bash
# IMPORTANT: MacOS users should run using Bash 4+ from Homebrew: `/usr/local/bin/bash script.sh`
# The default macOS Bash (3.2) does not support associative arrays used in this script
# Make sure this file is executable (chmod +x)
#
# OneFileLinux Build Profiles (86_build_profiles.sh)
# Defines the standard build profiles and their features
# This is the central source of truth for what each build profile includes
#

# Debug info about the script being loaded
echo -e "${BLUE}[DEBUG]${NC} Loading build profiles library $(basename "${BASH_SOURCE[0]}")"

# Variables to track available profiles
AVAILABLE_PROFILES="minimal standard full"

# Function to set minimal profile
set_minimal_profile() {
    INCLUDE_ZFS=false
    INCLUDE_BTRFS=false
    INCLUDE_RECOVERY_TOOLS=false
    INCLUDE_NETWORK_TOOLS=false
    INCLUDE_CRYPTO=false
    INCLUDE_TUI=false
    INCLUDE_MINIMAL_KERNEL=true
    INCLUDE_COMPRESSION=true
    INCLUDE_ADVANCED_FS=false
    INCLUDE_DISK_DIAG=false
    INCLUDE_NETWORK_DIAG=false
    INCLUDE_SYSTEM_TOOLS=false
    INCLUDE_DATA_RECOVERY=false
    INCLUDE_BOOT_REPAIR=false
    INCLUDE_EDITORS=false
    INCLUDE_SECURITY=false
    
    # Export variables
    export INCLUDE_ZFS INCLUDE_BTRFS INCLUDE_RECOVERY_TOOLS INCLUDE_NETWORK_TOOLS INCLUDE_CRYPTO
    export INCLUDE_TUI INCLUDE_MINIMAL_KERNEL INCLUDE_COMPRESSION INCLUDE_ADVANCED_FS INCLUDE_DISK_DIAG
    export INCLUDE_NETWORK_DIAG INCLUDE_SYSTEM_TOOLS INCLUDE_DATA_RECOVERY INCLUDE_BOOT_REPAIR
    export INCLUDE_EDITORS INCLUDE_SECURITY
    export BUILD_TYPE="minimal"
}

# Function to set standard profile
set_standard_profile() {
    INCLUDE_ZFS=true
    INCLUDE_BTRFS=false
    INCLUDE_RECOVERY_TOOLS=true
    INCLUDE_NETWORK_TOOLS=true
    INCLUDE_CRYPTO=true
    INCLUDE_TUI=true
    INCLUDE_MINIMAL_KERNEL=false
    INCLUDE_COMPRESSION=true
    INCLUDE_ADVANCED_FS=false
    INCLUDE_DISK_DIAG=false
    INCLUDE_NETWORK_DIAG=false
    INCLUDE_SYSTEM_TOOLS=false
    INCLUDE_DATA_RECOVERY=false
    INCLUDE_BOOT_REPAIR=false
    INCLUDE_EDITORS=false
    INCLUDE_SECURITY=false
    
    # Export variables
    export INCLUDE_ZFS INCLUDE_BTRFS INCLUDE_RECOVERY_TOOLS INCLUDE_NETWORK_TOOLS INCLUDE_CRYPTO
    export INCLUDE_TUI INCLUDE_MINIMAL_KERNEL INCLUDE_COMPRESSION INCLUDE_ADVANCED_FS INCLUDE_DISK_DIAG
    export INCLUDE_NETWORK_DIAG INCLUDE_SYSTEM_TOOLS INCLUDE_DATA_RECOVERY INCLUDE_BOOT_REPAIR
    export INCLUDE_EDITORS INCLUDE_SECURITY
    export BUILD_TYPE="standard"
}

# Function to set full profile
set_full_profile() {
    INCLUDE_ZFS=true
    INCLUDE_BTRFS=true
    INCLUDE_RECOVERY_TOOLS=true
    INCLUDE_NETWORK_TOOLS=true
    INCLUDE_CRYPTO=true
    INCLUDE_TUI=true
    INCLUDE_MINIMAL_KERNEL=false
    INCLUDE_COMPRESSION=true
    INCLUDE_ADVANCED_FS=true
    INCLUDE_DISK_DIAG=true
    INCLUDE_NETWORK_DIAG=true
    INCLUDE_SYSTEM_TOOLS=true
    INCLUDE_DATA_RECOVERY=true
    INCLUDE_BOOT_REPAIR=true
    INCLUDE_EDITORS=true
    INCLUDE_SECURITY=true
    
    # Export variables
    export INCLUDE_ZFS INCLUDE_BTRFS INCLUDE_RECOVERY_TOOLS INCLUDE_NETWORK_TOOLS INCLUDE_CRYPTO
    export INCLUDE_TUI INCLUDE_MINIMAL_KERNEL INCLUDE_COMPRESSION INCLUDE_ADVANCED_FS INCLUDE_DISK_DIAG
    export INCLUDE_NETWORK_DIAG INCLUDE_SYSTEM_TOOLS INCLUDE_DATA_RECOVERY INCLUDE_BOOT_REPAIR
    export INCLUDE_EDITORS INCLUDE_SECURITY
    export BUILD_TYPE="full"
}

# Function to apply a build profile - completely rewritten to avoid associative arrays
apply_build_profile() {
    local profile_name="$1"
    
    # Debug output for profile selection
    echo -e "${BLUE}[DEBUG]${NC} Trying to apply build profile: $profile_name"
    
    # Apply profile using direct function calls
    case "$profile_name" in
        "minimal")
            set_minimal_profile
            log "INFO" "Applied minimal build profile"
            return 0
            ;;
        "standard")
            set_standard_profile
            log "INFO" "Applied standard build profile"
            return 0
            ;;
        "full")
            set_full_profile
            log "INFO" "Applied full build profile"
            return 0
            ;;
        *)
            log "ERROR" "Unknown build profile: $profile_name"
            log "INFO" "Available profiles: $AVAILABLE_PROFILES"
            return 1
            ;;
    esac
}

# Function to determine the current build profile based on settings
get_build_profile_name() {
    # Check for minimal-like configuration
    if [ "${INCLUDE_MINIMAL_KERNEL}" = "true" ] && [ "${INCLUDE_ZFS}" = "false" ] && 
       [ "${INCLUDE_NETWORK_TOOLS}" = "false" ] && [ "${INCLUDE_CRYPTO}" = "false" ]; then
        echo "minimal"
        return 0
    fi
    
    # Check for full configuration - all features must be enabled
    if [ "${INCLUDE_ZFS}" = "true" ] && [ "${INCLUDE_BTRFS}" = "true" ] && 
       [ "${INCLUDE_RECOVERY_TOOLS}" = "true" ] && [ "${INCLUDE_NETWORK_TOOLS}" = "true" ] && 
       [ "${INCLUDE_CRYPTO}" = "true" ] && [ "${INCLUDE_TUI}" = "true" ] && 
       [ "${INCLUDE_ADVANCED_FS:-false}" = "true" ] && [ "${INCLUDE_DISK_DIAG:-false}" = "true" ] && 
       [ "${INCLUDE_NETWORK_DIAG:-false}" = "true" ] && [ "${INCLUDE_SYSTEM_TOOLS:-false}" = "true" ] && 
       [ "${INCLUDE_DATA_RECOVERY:-false}" = "true" ] && [ "${INCLUDE_BOOT_REPAIR:-false}" = "true" ] && 
       [ "${INCLUDE_EDITORS:-false}" = "true" ] && [ "${INCLUDE_SECURITY:-false}" = "true" ]; then
        echo "full"
        return 0
    fi
    
    # Check for standard configuration
    if [ "${INCLUDE_ZFS}" = "true" ] && [ "${INCLUDE_BTRFS}" = "false" ] && 
       [ "${INCLUDE_RECOVERY_TOOLS}" = "true" ] && [ "${INCLUDE_NETWORK_TOOLS}" = "true" ] && 
       [ "${INCLUDE_CRYPTO}" = "true" ] && [ "${INCLUDE_TUI}" = "true" ] && 
       [ "${INCLUDE_MINIMAL_KERNEL}" = "false" ] && 
       [ "${INCLUDE_ADVANCED_FS:-false}" = "false" ] && [ "${INCLUDE_DISK_DIAG:-false}" = "false" ] && 
       [ "${INCLUDE_NETWORK_DIAG:-false}" = "false" ] && [ "${INCLUDE_SYSTEM_TOOLS:-false}" = "false" ] && 
       [ "${INCLUDE_DATA_RECOVERY:-false}" = "false" ] && [ "${INCLUDE_BOOT_REPAIR:-false}" = "false" ] && 
       [ "${INCLUDE_EDITORS:-false}" = "false" ] && [ "${INCLUDE_SECURITY:-false}" = "false" ]; then
        echo "standard"
        return 0
    fi
    
    # If no exact match, determine the closest match or return "custom"
    if [ "${INCLUDE_MINIMAL_KERNEL}" = "true" ]; then
        echo "custom-minimal"
    elif [ "${INCLUDE_ZFS}" = "true" ] && [ "${INCLUDE_BTRFS}" = "true" ]; then
        echo "custom-full"
    else
        echo "custom"
    fi
}

# Function to list available build profiles
list_build_profiles() {
    log "INFO" "Available build profiles:"
    
    # Minimal profile
    log "INFO" "  - minimal"
    log "INFO" "    Features: Minimal Kernel, Compression"
    
    # Standard profile
    log "INFO" "  - standard"
    log "INFO" "    Features: ZFS, Recovery Tools, Network Tools, Crypto, TUI, Compression"
    
    # Full profile
    log "INFO" "  - full"
    log "INFO" "    Features: ZFS, Btrfs, Recovery Tools, Network Tools, Crypto, TUI, Compression, Advanced FS, Disk Diagnostics, Network Diagnostics, System Tools, Data Recovery, Boot Repair, Editors, Security"
}

# Export all functions for use in other scripts
export -f get_build_profile_name
export -f apply_build_profile
export -f list_build_profiles
export -f set_minimal_profile
export -f set_standard_profile
export -f set_full_profile