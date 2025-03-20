#!/bin/bash
# Make sure this file is executable (chmod +x)
#
# OneFileLinux Build Profiles (86_build_profiles.sh)
# Defines the standard build profiles and their features
# This is the central source of truth for what each build profile includes
#

# Initialize build profile definitions
declare -A BUILD_PROFILES

# Define the minimal profile
BUILD_PROFILES["minimal"]=$(cat << 'EOF'
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
EOF
)

# Define the standard profile
BUILD_PROFILES["standard"]=$(cat << 'EOF'
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
EOF
)

# Define the full profile
BUILD_PROFILES["full"]=$(cat << 'EOF'
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
EOF
)

# Function to determine the current build profile based on settings
get_build_profile_name() {
    # Check if it exactly matches a known profile
    for profile in "${!BUILD_PROFILES[@]}"; do
        # Create a temporary file with the current configuration
        local current_config=$(cat << EOF
INCLUDE_ZFS=${INCLUDE_ZFS}
INCLUDE_BTRFS=${INCLUDE_BTRFS}
INCLUDE_RECOVERY_TOOLS=${INCLUDE_RECOVERY_TOOLS}
INCLUDE_NETWORK_TOOLS=${INCLUDE_NETWORK_TOOLS}
INCLUDE_CRYPTO=${INCLUDE_CRYPTO}
INCLUDE_TUI=${INCLUDE_TUI}
INCLUDE_MINIMAL_KERNEL=${INCLUDE_MINIMAL_KERNEL}
INCLUDE_COMPRESSION=${INCLUDE_COMPRESSION}
INCLUDE_ADVANCED_FS=${INCLUDE_ADVANCED_FS:-false}
INCLUDE_DISK_DIAG=${INCLUDE_DISK_DIAG:-false}
INCLUDE_NETWORK_DIAG=${INCLUDE_NETWORK_DIAG:-false}
INCLUDE_SYSTEM_TOOLS=${INCLUDE_SYSTEM_TOOLS:-false}
INCLUDE_DATA_RECOVERY=${INCLUDE_DATA_RECOVERY:-false}
INCLUDE_BOOT_REPAIR=${INCLUDE_BOOT_REPAIR:-false}
INCLUDE_EDITORS=${INCLUDE_EDITORS:-false}
INCLUDE_SECURITY=${INCLUDE_SECURITY:-false}
EOF
)

        if [ "$current_config" = "${BUILD_PROFILES[$profile]}" ]; then
            echo "$profile"
            return 0
        fi
    done
    
    # If no exact match, determine the closest match or return "custom"
    if [ "${INCLUDE_MINIMAL_KERNEL}" = "true" ]; then
        echo "custom-minimal"
    elif [ "${INCLUDE_ZFS}" = "true" ] && [ "${INCLUDE_BTRFS}" = "true" ]; then
        echo "custom-full"
    else
        echo "custom"
    fi
}

# Function to apply a build profile
apply_build_profile() {
    local profile_name="$1"
    
    # Check if the profile exists
    if [ -z "${BUILD_PROFILES[$profile_name]}" ]; then
        log "ERROR" "Unknown build profile: $profile_name"
        return 1
    fi
    
    # Apply the profile by evaluating its configuration
    log "INFO" "Applying build profile: $profile_name"
    eval "${BUILD_PROFILES[$profile_name]}"
    
    # Just set BUILD_TYPE variable
    export BUILD_TYPE="$profile_name"
    log "INFO" "Applied build profile: $profile_name"
    
    return 0
}

# Function to list available build profiles
list_build_profiles() {
    log "INFO" "Available build profiles:"
    for profile in "${!BUILD_PROFILES[@]}"; do
        log "INFO" "  - $profile"
        # Parse the profile configuration to display features
        local features=""
        local config="${BUILD_PROFILES[$profile]}"
        
        [[ "$config" =~ INCLUDE_ZFS=true ]] && features+="ZFS, "
        [[ "$config" =~ INCLUDE_BTRFS=true ]] && features+="Btrfs, "
        [[ "$config" =~ INCLUDE_RECOVERY_TOOLS=true ]] && features+="Recovery Tools, "
        [[ "$config" =~ INCLUDE_NETWORK_TOOLS=true ]] && features+="Network Tools, "
        [[ "$config" =~ INCLUDE_CRYPTO=true ]] && features+="Crypto, "
        [[ "$config" =~ INCLUDE_TUI=true ]] && features+="TUI, "
        [[ "$config" =~ INCLUDE_MINIMAL_KERNEL=true ]] && features+="Minimal Kernel, "
        [[ "$config" =~ INCLUDE_COMPRESSION=true ]] && features+="Compression, "
        
        # Remove trailing comma and space
        features=${features%, }
        
        # Display features if they exist
        if [ -n "$features" ]; then
            log "INFO" "    Features: $features"
        fi
    done
}

# Export all functions for use in other scripts
export -f get_build_profile_name
export -f apply_build_profile
export -f list_build_profiles