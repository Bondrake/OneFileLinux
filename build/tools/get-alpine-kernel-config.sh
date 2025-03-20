#!/bin/bash
# Script to download and prepare Alpine Linux LTS kernel configuration

# Source the common library to get ALPINE_VERSION
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMMON_LIB="${SCRIPT_DIR}/../80_common.sh"

if [ -f "$COMMON_LIB" ]; then
    source "$COMMON_LIB"
else
    echo "ERROR: Common library not found at $COMMON_LIB"
    echo "This script must be run from the tools directory of the OneFileLinux build system."
    exit 1
fi

# Create the directory structure
setup_directories() {
    local base_dir="$1"
    
    # Create directories if they don't exist
    mkdir -p "$base_dir"/kernel-configs/{base,features}
    
    log "SUCCESS" "Created directory structure in $base_dir"
}

# Download Alpine LTS kernel config
download_alpine_config() {
    local base_dir="$1"
    local alpine_version="${2:-$ALPINE_VERSION}"  # Use the version from common library
    
    local config_url="https://git.alpinelinux.org/aports/plain/main/linux-lts/lts.x86_64.config"
    # Use kernel-configs as the idiomatic location for build-time configurations
    local output_file="$base_dir/kernel-configs/base/alpine-lts-$alpine_version.config"
    
    log "INFO" "Downloading Alpine Linux LTS kernel config from $config_url"
    
    # Create the directory if it doesn't exist
    mkdir -p "$(dirname "$output_file")"
    
    # Define multiple URLs to try (fallbacks) - only verified working URLs
    local urls=(
        "$config_url"
        "https://git.alpinelinux.org/aports/plain/main/linux-lts/lts.x86_64.config"
        "https://github.com/alpinelinux/aports/raw/refs/heads/master/main/linux-lts/lts.x86_64.config"
    )
    
    # Try each URL in sequence
    local download_success=false
    for url in "${urls[@]}"; do
        log "INFO" "Trying to download from: $url"
        
        # Use wget for downloading
        if command -v wget > /dev/null; then
            # Use verbose output for debugging during first attempt
            if [ "$url" = "$config_url" ]; then
                log "INFO" "First download attempt, using verbose output for wget"
                wget -v --timeout=30 --tries=3 -O "$output_file" "$url" || log "ERROR" "wget failed with exit code $?"
            else
                wget -q --timeout=30 --tries=3 -O "$output_file" "$url" || log "ERROR" "wget failed with exit code $?"
            fi
            
            # Check if the download produced a valid file
            if [ -s "$output_file" ]; then
                download_success=true
                log "SUCCESS" "Downloaded using wget from $url"
                break
            else
                log "ERROR" "wget download from $url produced empty file or failed"
            fi
        else
            log "ERROR" "wget is not available. Please install wget."
            return 1
        fi
    done
    
    # Check if download was successful
    if [ ! -f "$output_file" ] || [ ! -s "$output_file" ]; then
        log "ERROR" "Failed to download Alpine Linux LTS kernel config from all sources"
        return 1
    fi
    
    # Add header to the config file
    local temp_file=$(mktemp)
    echo "# Alpine Linux LTS kernel configuration" > "$temp_file"
    echo "# Source: $config_url" >> "$temp_file"
    echo "# Downloaded on $(date)" >> "$temp_file"
    echo "" >> "$temp_file"
    cat "$output_file" >> "$temp_file"
    mv "$temp_file" "$output_file"
    
    log "SUCCESS" "Downloaded Alpine Linux LTS kernel config to $output_file"
    
    # Create standard config
    mkdir -p "$base_dir/kernel-configs"
    cp "$output_file" "$base_dir/kernel-configs/standard.config"
    log "INFO" "Created standard config at $base_dir/kernel-configs/standard.config"
    
    # Create minimal config
    cp "$output_file" "$base_dir/kernel-configs/minimal.config"
    log "INFO" "Created minimal config at $base_dir/kernel-configs/minimal.config"
    
    # Verify files exist and have content
    if [ ! -s "$base_dir/kernel-configs/standard.config" ] || [ ! -s "$base_dir/kernel-configs/minimal.config" ]; then
        log "ERROR" "Config files not created properly or are empty"
        return 1
    fi
    
    return 0
}

# Minimize the kernel config
minimize_config() {
    local base_dir="$1"
    local minimize_script="$base_dir/minimize-kernel-config.sh"
    local minimal_config="$base_dir/kernel-configs/minimal.config"
    
    # Check if minimize script exists
    if [ ! -f "$minimize_script" ]; then
        log "ERROR" "Minimize script not found: $minimize_script"
        
        # Check if the script is in the tools directory
        if [ -f "$base_dir/tools/minimize-kernel-config.sh" ]; then
            log "INFO" "Found minimize script in tools directory, copying it"
            cp "$base_dir/tools/minimize-kernel-config.sh" "$minimize_script"
            chmod +x "$minimize_script"
        else
            log "WARNING" "Could not find minimize script, skipping minimization"
            return 0  # Continue without minimization
        fi
    fi
    
    # Make script executable if needed
    if [ ! -x "$minimize_script" ]; then
        chmod +x "$minimize_script"
    fi
    
    # Check if the minimal config exists
    if [ ! -f "$minimal_config" ]; then
        log "ERROR" "Kernel config file not found: $minimal_config"
        log "WARNING" "Cannot minimize non-existent config, skipping"
        return 0  # Continue without minimization
    fi
    
    # Run minimization
    log "INFO" "Minimizing kernel configuration"
    "$minimize_script" "$minimal_config" || {
        log "WARNING" "Minimization failed, but continuing with non-minimized config"
        return 0  # Continue without minimization
    }
    
    return 0  # Always return success to continue build
}

# Main function
main() {
    local base_dir="${1:-$(pwd)}"
    local alpine_version="${2:-3.17}"
    
    log "INFO" "Setting up Alpine-based kernel configuration in $base_dir"
    
    # Setup directory structure
    setup_directories "$base_dir"
    
    # Download Alpine kernel config
    download_alpine_config "$base_dir" "$alpine_version"
    
    # Copy minimize script if it exists in the current directory
    if [ -f "$(dirname "$0")/minimize-kernel-config.sh" ]; then
        cp "$(dirname "$0")/minimize-kernel-config.sh" "$base_dir/"
        log "INFO" "Copied minimize-kernel-config.sh to $base_dir/"
    fi
    
    # Minimize the kernel config if minimize script is available
    if [ -f "$base_dir/minimize-kernel-config.sh" ]; then
        minimize_config "$base_dir"
    else
        log "WARNING" "minimize-kernel-config.sh not found, skipping minimization"
    fi
    
    log "SUCCESS" "Alpine-based kernel configuration setup complete"
    log "INFO" "Standard config: $base_dir/kernel-configs/standard.config"
    log "INFO" "Minimal config: $base_dir/kernel-configs/minimal.config"
    
    return 0
}

# Run main function if script is executed directly
if [[ "$0" == "${BASH_SOURCE[0]}" ]]; then
    # Parse command line arguments
    BASE_DIR="$(pwd)"
    DEFAULT_ALPINE_VERSION="$ALPINE_VERSION"
    
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --dir=*)
                BASE_DIR="${1#*=}"
                shift
                ;;
            --version=*)
                ALPINE_VERSION="${1#*=}"
                shift
                ;;
            --help|-h)
                echo "Usage: $0 [--dir=PATH] [--version=VERSION]"
                echo ""
                echo "Options:"
                echo "  --dir=PATH      Directory to set up kernel configs (default: current directory)"
                echo "  --version=VERSION   Alpine Linux version (default: $DEFAULT_ALPINE_VERSION)"
                echo "  --help, -h      Show this help message"
                exit 0
                ;;
            *)
                echo "Unknown option: $1"
                echo "Use --help for usage information."
                exit 1
                ;;
        esac
    done
    
    main "$BASE_DIR" "$ALPINE_VERSION"
    exit $?
fi