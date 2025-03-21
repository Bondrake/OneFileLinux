#!/bin/bash
#
# OneFileLinux Build Helper (82_build_helper.sh)
# Cross-environment build utilities for consistent builds in any environment
# Provides file operations, environment detection, and build utilities
# This is part of the library scripts (80-89 range)
#

# Detect environment types
is_github_actions() {
    [ -n "${GITHUB_ACTIONS:-}" ]
}

is_docker_container() {
    [ -n "${IN_DOCKER_CONTAINER:-}" ] || grep -q "docker\|container" /proc/1/cgroup 2>/dev/null || [ -f "/.dockerenv" ]
}

is_restricted_environment() {
    is_github_actions || is_docker_container
}

# Print environment information
print_environment_info() {
    echo "OneFileLinux Build Environment:"
    echo "------------------------------"
    
    if is_github_actions; then
        echo "GitHub Actions: YES"
        echo "Runner OS: ${RUNNER_OS:-Unknown}"
        echo "GitHub Repository: ${GITHUB_REPOSITORY:-Unknown}"
    else
        echo "GitHub Actions: NO"
    fi
    
    if is_docker_container; then
        echo "Docker Container: YES"
    else
        echo "Docker Container: NO"
    fi
    
    echo "Running as user: $(id -un) ($(id -u))"
    echo "System: $(uname -s) $(uname -m)"
    echo "CPU Cores: $(nproc 2>/dev/null || echo "Unknown")"
    echo "------------------------------"
}

# Fix permissions for scripts in a directory
fix_script_permissions() {
    local dir=$1
    
    if [ -d "$dir" ]; then
        echo "Fixing script permissions in $dir"
        find "$dir" -name "*.sh" -exec chmod +x {} \; 2>/dev/null || true
        
        if [ -d "$dir/scripts" ]; then
            chmod -R +x "$dir/scripts" 2>/dev/null || true
        fi
        
        # Make common build scripts executable
        for script in configure autogen.sh bootstrap.sh; do
            if [ -f "$dir/$script" ]; then
                chmod +x "$dir/$script" 2>/dev/null || true
            fi
        done
    fi
}

# Create necessary directories with proper permissions
ensure_directory() {
    local dir=$1
    local mode=${2:-755}
    
    if [ ! -d "$dir" ]; then
        echo "Creating directory: $dir"
        mkdir -p "$dir"
        chmod "$mode" "$dir"
    fi
}

# Configure Alpine Linux with proper error handling
configure_alpine() {
    local rootfs_dir=$1
    local zfiles_dir=$2
    
    if [ ! -d "$rootfs_dir" ]; then
        echo "Error: Alpine root directory not found: $rootfs_dir"
        return 1
    fi
    
    # Create runlevel directories with proper permissions
    mkdir -p "$rootfs_dir/etc/runlevels/sysinit"
    chmod 777 "$rootfs_dir/etc/runlevels/sysinit"
    
    # Create service symlinks
    for service in mdev devfs dmesg syslog hwdrivers networking; do
        ln -sf "/etc/init.d/$service" "$rootfs_dir/etc/runlevels/sysinit/$service"
    done
    
    # Set up terminal access
    ln -sf "/sbin/agetty" "$rootfs_dir/sbin/getty"
    
    # Copy configuration files if zfiles directory exists
    if [ -d "$zfiles_dir" ]; then
        # Create network directory
        mkdir -p "$rootfs_dir/etc/network"
        
        # Copy files if they exist
        if [ -f "$zfiles_dir/interfaces" ]; then
            cp "$zfiles_dir/interfaces" "$rootfs_dir/etc/network/interfaces"
            chmod 644 "$rootfs_dir/etc/network/interfaces"
        else
            echo "Creating default interfaces file"
            echo "auto lo" > "$rootfs_dir/etc/network/interfaces"
            echo "iface lo inet loopback" >> "$rootfs_dir/etc/network/interfaces"
            chmod 644 "$rootfs_dir/etc/network/interfaces"
        fi
        
        if [ -f "$zfiles_dir/resolv.conf" ]; then
            cp "$zfiles_dir/resolv.conf" "$rootfs_dir/etc/resolv.conf"
            chmod 644 "$rootfs_dir/etc/resolv.conf"
        else
            echo "Creating default resolv.conf"
            echo "nameserver 8.8.8.8" > "$rootfs_dir/etc/resolv.conf"
            chmod 644 "$rootfs_dir/etc/resolv.conf"
        fi
        
        if [ -f "$zfiles_dir/profile" ]; then
            cp "$zfiles_dir/profile" "$rootfs_dir/etc/profile"
            chmod 644 "$rootfs_dir/etc/profile"
        fi
        
        if [ -f "$zfiles_dir/shadow" ]; then
            cp "$zfiles_dir/shadow" "$rootfs_dir/etc/shadow"
            chmod 644 "$rootfs_dir/etc/shadow"
        fi
        
        if [ -f "$zfiles_dir/init" ]; then
            cp "$zfiles_dir/init" "$rootfs_dir/init"
            chmod 755 "$rootfs_dir/init"
        fi
        
        if [ -f "$zfiles_dir/onefilelinux-tui" ]; then
            cp "$zfiles_dir/onefilelinux-tui" "$rootfs_dir/onefilelinux-tui"
            chmod 755 "$rootfs_dir/onefilelinux-tui"
        fi
    else
        echo "Warning: zfiles directory not found: $zfiles_dir"
    fi
    
    # Configure console settings if inittab exists
    if [ -f "$rootfs_dir/etc/inittab" ]; then
        sed -i 's/^#ttyS0/ttyS0/' "$rootfs_dir/etc/inittab"
        sed -i 's|\(/sbin/getty \)|\1 -a root |' "$rootfs_dir/etc/inittab"
    fi
    
    return 0
}

# Setup kernel configuration
setup_kernel_config() {
    local kernel_dir=$1
    local config_type=$2
    local zfiles_dir=$3
    
    if [ ! -d "$kernel_dir" ]; then
        echo "Error: Kernel directory not found: $kernel_dir"
        return 1
    fi
    
    # Fix script permissions
    fix_script_permissions "$kernel_dir"
    
    # Check for kernel-configs directory structure
    local config_base="${BUILD_DIR:-$workdir}/kernel-configs"
    
    # Choose the appropriate base config
    case "${config_type:-standard}" in
        "minimal")
            base_config="$config_base/minimal.config"
            ;;
        "standard"|*)
            base_config="$config_base/standard.config"
            ;;
    esac
    
    # Check for existing config
    if [ ! -f "$kernel_dir/.config" ]; then
        echo "Kernel config not found, creating one"
        
        if [ -f "$base_config" ]; then
            echo "Copying Alpine kernel config: $base_config"
            cp "$base_config" "$kernel_dir/.config"
            
            # Apply config overlays if feature directory exists
            local feature_dir="$config_base/features"
            if [ -d "$feature_dir" ]; then
                echo "Applying feature config overlays:"
                
                # Apply ZFS support configs if explicitly enabled
                if [ "${INCLUDE_ZFS:-true}" = "true" ] && [ -f "$feature_dir/zfs-support.conf" ]; then
                    echo "- Applying ZFS support config"
                    cat "$feature_dir/zfs-support.conf" >> "$kernel_dir/.config"
                fi
            fi
            
            # Run olddefconfig to ensure the config is valid
            (cd "$kernel_dir" && make olddefconfig)
        else
            echo "Error: Alpine kernel config not found: $base_config"
            return 1
        fi
    fi
    
    return 0
}

# Setup and configure ZFS build environment
setup_zfs() {
    local zfs_dir=$1
    local kernel_dir=$2
    
    if [ ! -d "$zfs_dir" ]; then
        echo "Error: ZFS directory not found: $zfs_dir"
        return 1
    fi
    
    # Fix script permissions
    fix_script_permissions "$zfs_dir"
    
    # Ensure autogen.sh and configure are executable
    if [ -f "$zfs_dir/autogen.sh" ]; then
        chmod +x "$zfs_dir/autogen.sh"
    fi
    
    if [ -f "$zfs_dir/configure" ]; then
        chmod +x "$zfs_dir/configure"
    fi
    
    # Run autogen.sh if configure doesn't exist
    if [ ! -f "$zfs_dir/configure" ]; then
        echo "Running autogen.sh to generate configure script"
        (cd "$zfs_dir" && ./autogen.sh)
    fi
    
    return 0
}

# A silent version of log that doesn't affect stdout
_log_silent() {
    local level=$1
    local message=$2
    # Log to stderr instead
    >&2 echo -e "${BLUE}[INFO]${NC} $message"
}

# Determine optimal number of build threads (completely revised for safety)
get_optimal_threads() {
    # Keep any logs out of stdout to avoid affecting the return value
    exec 3>&1  # Save the current stdout
    exec 1>&2  # Redirect stdout to stderr
    
    local available_memory_kb=0
    local total_cores=0
    local result=2  # Default fallback value
    
    # Detect available memory
    if [ -f "/proc/meminfo" ]; then
        available_memory_kb=$(grep MemAvailable /proc/meminfo 2>/dev/null | awk '{print $2}' || echo 0)
    fi
    
    # If memory detection failed, try total memory with a reduction factor
    if [ "$available_memory_kb" -eq 0 ] && [ -f "/proc/meminfo" ]; then
        local total_memory_kb=$(grep MemTotal /proc/meminfo 2>/dev/null | awk '{print $2}' || echo 0)
        available_memory_kb=$((total_memory_kb * 7 / 10)) # Use 70% of total memory
    fi
    
    # Convert to GB for display and debugging
    local available_memory_gb=$(awk "BEGIN {printf \"%.1f\", $available_memory_kb/1024/1024}")
    log "INFO" "Available memory: ${available_memory_gb}GB"
    
    # Detect total cores
    total_cores=$(nproc 2>/dev/null || echo 2)
    log "INFO" "System has $total_cores CPU threads available"
    
    # Calculate a safe number of threads based on available memory
    # Empirically, each compilation thread needs ~2GB for kernel compilation
    if [ "$available_memory_kb" -gt 0 ]; then
        local safe_threads=$(awk "BEGIN {print int($available_memory_kb/1024/1024/2)}")
        
        # Ensure at least 1 thread and no more than total cores
        safe_threads=$(( safe_threads < 1 ? 1 : safe_threads ))
        safe_threads=$(( safe_threads > total_cores ? total_cores : safe_threads ))
        
        # Special handling for extremely low memory (< 4GB) environments
        if [ "$available_memory_kb" -lt 4000000 ]; then
            log "WARNING" "Very low memory environment detected (${available_memory_gb}GB)"
            
            # In extremely low memory environments, limit to 2 threads max
            if [ "$safe_threads" -gt 2 ]; then
                log "WARNING" "Reducing threads from $safe_threads to 2 due to very low memory"
                safe_threads=2
            fi
            
            # In GitHub Actions, be even more conservative with 1 thread
            if is_github_actions && [ "$safe_threads" -gt 1 ]; then
                log "WARNING" "GitHub Actions with very low memory: limiting to 1 thread for reliability"
                safe_threads=1
            fi
        fi
        
        log "INFO" "Using $safe_threads build threads (based on available memory)"
        result=$safe_threads
    else
        # Default to 2 threads if memory detection failed
        log "WARNING" "Could not detect available memory. Using conservative thread count: 2"
        result=2
    fi
    
    # Restore stdout
    exec 1>&3
    exec 3>&-
    
    # Return only the raw number on stdout
    echo "$result"
}

# Get compiler flags optimized for the current memory environment
get_memory_optimized_cflags() {
    # Keep any logs out of stdout to avoid affecting the return value
    exec 3>&1  # Save the current stdout
    exec 1>&2  # Redirect stdout to stderr
    
    local available_memory_kb=0
    local result="-O2"  # Default optimization
    
    # Detect available memory
    if [ -f "/proc/meminfo" ]; then
        available_memory_kb=$(grep MemAvailable /proc/meminfo 2>/dev/null | awk '{print $2}' || echo 0)
    fi
    
    # If memory detection failed, try total memory with a reduction factor
    if [ "$available_memory_kb" -eq 0 ] && [ -f "/proc/meminfo" ]; then
        local total_memory_kb=$(grep MemTotal /proc/meminfo 2>/dev/null | awk '{print $2}' || echo 0)
        available_memory_kb=$((total_memory_kb * 7 / 10)) # Use 70% of total memory
    fi
    
    # Convert to GB for display and debugging
    local available_memory_gb=$(awk "BEGIN {printf \"%.1f\", $available_memory_kb/1024/1024}")
    
    # Determine compiler optimization flags based on memory
    if [ "$available_memory_kb" -lt 4000000 ]; then  # < 4GB
        log "WARNING" "Very low memory (${available_memory_gb}GB), using minimal optimization"
        result="-g0 -Os -fno-inline"  # Optimize for minimal memory usage
    elif [ "$available_memory_kb" -lt 8000000 ]; then  # < 8GB
        log "INFO" "Low memory environment detected (${available_memory_gb}GB), using memory-saving options"
        result="-g0 -Os"  # Optimize for size, omit debug info
    else
        # Default optimization for systems with adequate memory
        result="-O2"
    fi
    
    # Restore stdout
    exec 1>&3
    exec 3>&-
    
    # Return only the raw flags on stdout
    echo "$result"
}

# Print a section header
print_section() {
    local title=$1
    echo "===================================================================="
    echo "  $title"
    echo "===================================================================="
}

# Use print_banner from 80_common.sh instead of duplicating it here

# Docker-specific extraction handling function
docker_handle_extraction() {
    local src_file="$1"
    local dest_dir="$2"
    
    log "INFO" "Special handling for extraction of $src_file to $dest_dir"
    
    # Create temporary extraction directory
    local temp_extract_dir="/tmp/onefilelinux_extract_temp"
    mkdir -p "$temp_extract_dir"
    
    # Create destination directory first
    mkdir -p "$dest_dir"
    
    # Extract as root first to a temp location
    if [[ "$src_file" == *.tar.gz ]]; then
        tar -xzf "$src_file" -C "$temp_extract_dir"
    elif [[ "$src_file" == *.tar.xz ]]; then
        # For .tar.xz files, must decompress and extract separately
        xz -dc "$src_file" | tar -x -C "$temp_extract_dir"
    fi
    
    # Check if extraction succeeded
    if [ $? -ne 0 ] || [ -z "$(ls -A "$temp_extract_dir")" ]; then
        log "WARNING" "Initial extraction failed, trying alternative method"
        # Try using tar with xf directly
        if [[ "$src_file" == *.tar.xz ]]; then
            tar -xf "$src_file" -C "$temp_extract_dir"
        fi
    fi
    
    # Check again if extraction succeeded
    if [ -z "$(ls -A "$temp_extract_dir")" ]; then
        log "ERROR" "All extraction methods failed for $src_file"
        return 1
    fi
    
    # Copy files to final location with correct permissions
    cp -a "$temp_extract_dir"/* "$dest_dir"/ || true
    
    # Clean up
    rm -rf "$temp_extract_dir"/*
    
    # Verify final directory has content
    if [ -z "$(ls -A "$dest_dir")" ]; then
        log "ERROR" "Failed to extract content to $dest_dir"
        return 1
    fi
    
    log "SUCCESS" "Extraction completed successfully to $dest_dir"
    return 0
}

# Docker-specific Alpine minirootfs preparation
prepare_alpine_minirootfs() {
    local rootfs_dir=${1:-"alpine-minirootfs"}
    
    if [ ! -d "$rootfs_dir" ]; then
        log "WARNING" "Alpine minirootfs directory not found: $rootfs_dir"
        return 1
    fi
    
    log "INFO" "Preparing Alpine minirootfs for Docker environment"
    
    # Ensure permissions allow writing to all subdirectories
    find "$rootfs_dir" -type d -exec chmod 755 {} \;
    
    # Make sure root-owned directories are writable by all users
    # This is necessary for the build scripts to create symlinks
    mkdir -p "$rootfs_dir/etc/runlevels/sysinit"
    chmod -R 777 "$rootfs_dir/etc/runlevels"
    
    log "SUCCESS" "Set proper permissions on Alpine minirootfs"
    return 0
}

# Cross-environment chroot helpers
# -------------------------------------

# Prepares a chroot environment with proper mount handling based on environment
prepare_chroot() {
    local chroot_dir="$1"
    local mount_special=${2:-true}
    
    # Make sure the directory exists
    if [ ! -d "$chroot_dir" ]; then
        log "ERROR" "Chroot directory not found: $chroot_dir"
        return 1
    fi
    
    log "INFO" "Preparing chroot environment: $chroot_dir"
    
    # Create essential directories
    mkdir -p "$chroot_dir/proc" "$chroot_dir/sys" "$chroot_dir/dev" "$chroot_dir/dev/pts"
    
    # Check if we're running in a Docker container
    if is_docker_container; then
        log "INFO" "Running in Docker container - using simplified chroot preparation"
        
        # In Docker, just mount proc if requested and it's not already mounted
        if [ "$mount_special" = true ] && ! mountpoint -q "$chroot_dir/proc" 2>/dev/null; then
            log "INFO" "Mounting proc filesystem"
            mount -t proc none "$chroot_dir/proc" 2>/dev/null || log "WARNING" "Could not mount proc (this is normal in Docker)"
        fi
        
        # Set safe permissions on special directories
        chmod 555 "$chroot_dir/proc"
        if [ -d "$chroot_dir/var/empty" ]; then
            chmod 555 "$chroot_dir/var/empty"
        fi
    else
        # Standard non-Docker environment
        log "INFO" "Preparing standard chroot environment"
        
        if [ "$mount_special" = true ]; then
            # Mount special filesystems if they're not already mounted
            if ! mountpoint -q "$chroot_dir/proc" 2>/dev/null; then
                mount -t proc none "$chroot_dir/proc" || log "WARNING" "Could not mount proc"
            fi
            
            if ! mountpoint -q "$chroot_dir/sys" 2>/dev/null; then
                mount -t sysfs none "$chroot_dir/sys" || log "WARNING" "Could not mount sysfs"
            fi
            
            if ! mountpoint -q "$chroot_dir/dev" 2>/dev/null; then
                mount -o bind /dev "$chroot_dir/dev" || log "WARNING" "Could not bind mount /dev"
            fi
            
            if ! mountpoint -q "$chroot_dir/dev/pts" 2>/dev/null; then
                mount -o bind /dev/pts "$chroot_dir/dev/pts" || log "WARNING" "Could not bind mount /dev/pts"
            fi
        fi
    fi
    
    # Copy resolv.conf for network connectivity
    if [ -f "/etc/resolv.conf" ]; then
        cp "/etc/resolv.conf" "$chroot_dir/etc/resolv.conf" 2>/dev/null || log "WARNING" "Could not copy resolv.conf"
    fi
    
    return 0
}

# Cleans up a chroot environment by unmounting filesystems
cleanup_chroot() {
    local chroot_dir="$1"
    
    log "INFO" "Cleaning up chroot environment: $chroot_dir"
    
    # Check if we're running in a Docker container
    if is_docker_container; then
        log "INFO" "Running in Docker container - using simplified chroot cleanup"
        
        # In Docker, just try to unmount proc
        if mountpoint -q "$chroot_dir/proc" 2>/dev/null; then
            umount "$chroot_dir/proc" 2>/dev/null || log "WARNING" "Could not unmount proc (this is normal in Docker)"
        fi
    else
        # Normal full chroot cleanup
        log "INFO" "Cleaning up standard chroot environment"
        
        # Unmount special filesystems in reverse order
        if mountpoint -q "$chroot_dir/dev/pts" 2>/dev/null; then
            umount "$chroot_dir/dev/pts" 2>/dev/null || log "WARNING" "Could not unmount dev/pts"
        fi
        
        if mountpoint -q "$chroot_dir/dev" 2>/dev/null; then
            umount "$chroot_dir/dev" 2>/dev/null || log "WARNING" "Could not unmount dev"
        fi
        
        if mountpoint -q "$chroot_dir/sys" 2>/dev/null; then
            umount "$chroot_dir/sys" 2>/dev/null || log "WARNING" "Could not unmount sys"
        fi
        
        if mountpoint -q "$chroot_dir/proc" 2>/dev/null; then
            umount "$chroot_dir/proc" 2>/dev/null || log "WARNING" "Could not unmount proc"
        fi
    fi
    
    return 0
}

# Runs a command in the chroot environment
run_in_chroot() {
    local chroot_dir="$1"
    local command="$2"
    local mount_special=${3:-true}
    
    # Prepare the chroot environment
    prepare_chroot "$chroot_dir" "$mount_special"
    
    # Run the command
    log "INFO" "Running command in chroot: $command"
    chroot "$chroot_dir" /bin/sh -c "$command"
    local exit_code=$?
    
    # Clean up the chroot environment
    cleanup_chroot "$chroot_dir"
    
    return $exit_code
}

# Get the latest Alpine Linux minor version for a major version
get_latest_alpine_version() {
    local base_version="${1:-$ALPINE_VERSION}"  # Default to global ALPINE_VERSION
    local fallback_patch="${2:-3}"              # Default patch version if check fails
    local timeout_seconds="${3:-5}"             # Default timeout of 5 seconds
    local quiet="${4:-false}"                  # Whether to suppress logging
    
    # Extract major.minor version
    local major_minor=$(echo "$base_version" | grep -oE '^[0-9]+\.[0-9]+')
    if [ -z "$major_minor" ]; then
        [ "$quiet" != "true" ] && log "ERROR" "Invalid Alpine version format: $base_version"
        echo "${base_version}.${fallback_patch}"
        return 1
    fi
    
    [ "$quiet" != "true" ] && log "INFO" "Checking for latest Alpine $major_minor.x release..."
    
    # Create a temporary file for the version information
    local tmp_file=$(mktemp)
    
    # Try to get the latest version from the Alpine website
    if timeout "$timeout_seconds" wget -q -O "$tmp_file" "https://alpinelinux.org/downloads/" 2>/dev/null; then
        # Extract the latest version for the major.minor series
        local latest_version=$(grep -oE "alpine-minirootfs-${major_minor}\.[0-9]+-x86_64\.tar\.gz" "$tmp_file" | sort -V | tail -n1 | grep -oE "${major_minor}\.[0-9]+" || echo "")
        
        # Clean up
        rm -f "$tmp_file"
        
        if [ -n "$latest_version" ]; then
            [ "$quiet" != "true" ] && log "SUCCESS" "Found latest Alpine version: $latest_version"
            echo "$latest_version"
            return 0
        fi
    else
        [ "$quiet" != "true" ] && log "WARNING" "Failed to check for latest Alpine version (timeout)"
    fi
    
    # Clean up in case of failure
    rm -f "$tmp_file" 2>/dev/null || true
    
    # Fall back to default patch version
    local fallback_version="${major_minor}.${fallback_patch}"
    [ "$quiet" != "true" ] && log "INFO" "Using fallback Alpine version: $fallback_version"
    echo "$fallback_version"
    return 0
}

# Function to get Alpine minirootfs URL
get_alpine_minirootfs_url() {
    local version="${1:-$ALPINE_VERSION}"
    local arch="${2:-x86_64}"
    local quiet="${3:-false}"
    
    # If version is just major.minor, try to get the latest patch version
    if [[ "$version" =~ ^[0-9]+\.[0-9]+$ ]]; then
        # Use quiet=true to avoid log messages interfering with URL output
        version=$(get_latest_alpine_version "$version" "3" "5" "true")
    fi
    
    # Clean and parse the version properly
    local major_minor=$(echo "$version" | grep -oE '^[0-9]+\.[0-9]+')
    if [ -z "$major_minor" ]; then
        # Use stderr for logging to avoid capturing in output
        [ "$quiet" != "true" ] && log "ERROR" "Invalid Alpine version format after resolution: $version" >&2
        # Fallback to a known good version format
        major_minor="3.21"
        version="3.21.3"
    fi
    
    local alpine_file="alpine-minirootfs-${version}-${arch}.tar.gz"
    local alpine_url="https://dl-cdn.alpinelinux.org/alpine/v${major_minor}/releases/${arch}/${alpine_file}"
    
    echo "$alpine_url"
}

# Check if extraction is needed based on target directory contents
needs_extraction() {
    local target_dir="$1"
    local component_type="$2"
    
    # Return true if target directory doesn't exist (needs extraction)
    if [ ! -d "$target_dir" ]; then
        return 0
    fi
    
    # Check for extraction completion marker
    if [ -f "$target_dir/.extraction_complete" ]; then
        log "INFO" "Component already extracted (found marker file)"
        return 1
    fi
    
    # Component-specific checks
    case "$component_type" in
        "kernel")
            # For Linux kernel, check if key files exist that would indicate extraction is complete
            if [ -f "$target_dir/Makefile" ] && [ -d "$target_dir/scripts" ]; then
                log "INFO" "Linux kernel appears to be already extracted"
                # Create the marker file for future checks
                touch "$target_dir/.extraction_complete"
                return 1
            fi
            ;;
        "alpine")
            # For Alpine Linux, check if /etc/alpine-release exists
            if [ -f "$target_dir/etc/alpine-release" ]; then
                log "INFO" "Alpine Linux appears to be already extracted"
                # Create the marker file for future checks
                touch "$target_dir/.extraction_complete"
                return 1
            fi
            ;;
        "zfs")
            # For ZFS, check if configure or autogen.sh exists
            if [ -f "$target_dir/configure" ] || [ -f "$target_dir/autogen.sh" ]; then
                log "INFO" "OpenZFS appears to be already extracted"
                # Create the marker file for future checks
                touch "$target_dir/.extraction_complete"
                return 1
            fi
            ;;
        *)
            # For unknown components, check if directory is not empty
            if [ "$(ls -A "$target_dir" 2>/dev/null)" ]; then
                log "INFO" "Directory not empty, assuming already extracted"
                # Create the marker file for future checks
                touch "$target_dir/.extraction_complete"
                return 1
            fi
            ;;
    esac
    
    # If we reach here, extraction is needed
    return 0
}

# Mark extraction as complete
mark_extraction_complete() {
    local target_dir="$1"
    
    if [ -d "$target_dir" ]; then
        touch "$target_dir/.extraction_complete"
        log "INFO" "Marked extraction as complete"
    fi
}

# Fix kernel permissions to avoid build errors
fix_kernel_permissions() {
    local kernel_dir="${1:-linux}"
    log "INFO" "Ensuring proper permissions for kernel source at $kernel_dir"
    
    if [ ! -d "$kernel_dir" ]; then
        log "WARNING" "Kernel directory not found: $kernel_dir"
        return 1
    fi
    
    # Check if we're in a container environment
    if is_docker_container || is_restricted_environment; then
        log "INFO" "Detected container environment, applying special permission fixes"
        
        # Fix directory permissions with u+rwx
        find "$kernel_dir" -type d -not -perm -u+rwx -exec chmod u+rwx {} \; 2>/dev/null || true
        
        # Fix file permissions with u+rw
        find "$kernel_dir" -type f -not -perm -u+rw -exec chmod u+rw {} \; 2>/dev/null || true
        
        # Specifically handle critical directories
        for dir in include arch scripts tools; do
            if [ -d "$kernel_dir/$dir" ]; then
                log "INFO" "Fixing permissions for $kernel_dir/$dir"
                chmod -R u+rwX "$kernel_dir/$dir" 2>/dev/null || true
            fi
        done
        
        # Ensure scripts are executable
        find "$kernel_dir/scripts" -name "*.sh" -exec chmod +x {} \; 2>/dev/null || true
    else
        # For non-container environments, just ensure scripts are executable
        find "$kernel_dir/scripts" -name "*.sh" -exec chmod +x {} \; 2>/dev/null || true
    fi
    
    log "SUCCESS" "Kernel source permissions fixed"
    return 0
}

# Parse build flags to set feature variables consistently across all scripts
parse_build_flags() {
    local build_args="$1"
    local apply_changes=${2:-true}  # Whether to apply changes to env vars

    # Use a temporary log function to avoid affecting stdout
    function _temp_log {
        >&2 echo -e "${BLUE}[INFO]${NC} $1"
    }

    # Add explicit debug for argument handling
    echo "DEBUG: parse_build_flags received args: '$build_args'" >&2
    # Print the raw arguments without splitting to see if quotes are preserved
    echo "DEBUG: Raw arguments: $build_args" >&2
    
    _temp_log "Parsing build flags: $build_args"
    
    # Default flag values
    local _include_minimal_kernel=false
    local _include_zfs=true
    local _include_btrfs=false 
    local _include_recovery_tools=true
    local _include_network_tools=true
    local _include_crypto=true
    local _include_tui=true
    local _include_advanced_fs=false
    local _include_disk_diag=false
    local _include_network_diag=false
    local _include_system_tools=false
    local _include_data_recovery=false
    local _include_boot_repair=false
    local _include_editors=false
    local _include_security=false
    
    # Read current env vars if they're set
    if [ -n "${INCLUDE_MINIMAL_KERNEL:-}" ]; then _include_minimal_kernel="$INCLUDE_MINIMAL_KERNEL"; fi
    if [ -n "${INCLUDE_ZFS:-}" ]; then _include_zfs="$INCLUDE_ZFS"; fi
    if [ -n "${INCLUDE_BTRFS:-}" ]; then _include_btrfs="$INCLUDE_BTRFS"; fi
    if [ -n "${INCLUDE_RECOVERY_TOOLS:-}" ]; then _include_recovery_tools="$INCLUDE_RECOVERY_TOOLS"; fi
    if [ -n "${INCLUDE_NETWORK_TOOLS:-}" ]; then _include_network_tools="$INCLUDE_NETWORK_TOOLS"; fi
    if [ -n "${INCLUDE_CRYPTO:-}" ]; then _include_crypto="$INCLUDE_CRYPTO"; fi
    if [ -n "${INCLUDE_TUI:-}" ]; then _include_tui="$INCLUDE_TUI"; fi
    if [ -n "${INCLUDE_ADVANCED_FS:-}" ]; then _include_advanced_fs="$INCLUDE_ADVANCED_FS"; fi
    if [ -n "${INCLUDE_DISK_DIAG:-}" ]; then _include_disk_diag="$INCLUDE_DISK_DIAG"; fi
    if [ -n "${INCLUDE_NETWORK_DIAG:-}" ]; then _include_network_diag="$INCLUDE_NETWORK_DIAG"; fi
    if [ -n "${INCLUDE_SYSTEM_TOOLS:-}" ]; then _include_system_tools="$INCLUDE_SYSTEM_TOOLS"; fi
    if [ -n "${INCLUDE_DATA_RECOVERY:-}" ]; then _include_data_recovery="$INCLUDE_DATA_RECOVERY"; fi
    if [ -n "${INCLUDE_BOOT_REPAIR:-}" ]; then _include_boot_repair="$INCLUDE_BOOT_REPAIR"; fi
    if [ -n "${INCLUDE_EDITORS:-}" ]; then _include_editors="$INCLUDE_EDITORS"; fi
    if [ -n "${INCLUDE_SECURITY:-}" ]; then _include_security="$INCLUDE_SECURITY"; fi
    
    # Split the build_args string into an array with better handling for quoted arguments
    # First normalize spaces around delimiters to ensure proper splitting
    local normalized_args=$(echo "$build_args" | sed 's/--/ --/g' | sed 's/^--/--/g')
    echo "DEBUG: Normalized args: $normalized_args" >&2
    
    # Split into array
    IFS=' ' read -ra FLAGS <<< "$normalized_args"
    
    # Log the flags for debugging
    echo "DEBUG: Split flags:" >&2
    for i in "${!FLAGS[@]}"; do
        echo "DEBUG:   [$i] = ${FLAGS[$i]}" >&2
    done
    
    # Iterate through all flags
    for flag in "${FLAGS[@]}"; do
        # Skip empty flags
        [ -z "$flag" ] && continue
        
        # Trim leading/trailing whitespace
        flag=$(echo "$flag" | xargs)
        case "$flag" in
            --minimal)
                echo "DEBUG: parse_build_flags detected --minimal flag, applying minimal configuration" >&2
                _temp_log "Detected --minimal flag, applying minimal configuration"
                
                # Apply minimal configuration using the library function
                if type -t set_minimal_profile &>/dev/null; then
                    # Use the dedicated function from build_profiles if available
                    echo "DEBUG: Using set_minimal_profile function from profiles library" >&2
                    set_minimal_profile
                    echo "DEBUG: Successfully applied minimal profile using dedicated function" >&2
                fi
                ;;
            --standard)
                echo "DEBUG: parse_build_flags detected --standard flag, applying standard configuration" >&2
                _temp_log "Detected --standard flag, applying standard configuration"
                
                # Apply standard configuration using the library function
                if type -t set_standard_profile &>/dev/null; then
                    # Use the dedicated function from build_profiles if available
                    echo "DEBUG: Using set_standard_profile function from profiles library" >&2
                    set_standard_profile
                    echo "DEBUG: Successfully applied standard profile using dedicated function" >&2
                fi
                ;;
            --full)
                echo "DEBUG: parse_build_flags detected --full flag, applying full configuration" >&2
                _temp_log "Detected --full flag, applying full configuration"
                
                # Apply full configuration using the library function
                if type -t set_full_profile &>/dev/null; then
                    # Use the dedicated function from build_profiles if available
                    set_full_profile
                    echo "DEBUG: Applied full profile using dedicated function" >&2
                fi
                ;;
            --profile=*)
                profile_name="${flag#*=}"
                echo "DEBUG: parse_build_flags detected --profile=$profile_name flag" >&2
                _temp_log "Detected --profile=$profile_name flag, applying profile"
                
                # Use apply_build_profile from the build_profiles library
                if type -t apply_build_profile &>/dev/null; then
                    echo "DEBUG: Using apply_build_profile function to apply $profile_name profile" >&2
                    apply_build_profile "$profile_name"
                fi
                ;;
            --minimal-kernel)
                _include_minimal_kernel=true
                ;;
            --standard-kernel)
                _include_minimal_kernel=false
                ;;
            --with-zfs)
                _include_zfs=true
                ;;
            --without-zfs)
                _include_zfs=false
                ;;
            --with-btrfs)
                _include_btrfs=true
                ;;
            --without-btrfs)
                _include_btrfs=false
                ;;
            --with-recovery-tools)
                _include_recovery_tools=true
                ;;
            --without-recovery-tools)
                _include_recovery_tools=false
                ;;
            --with-network-tools)
                _include_network_tools=true
                ;;
            --without-network-tools)
                _include_network_tools=false
                ;;
            --with-crypto)
                _include_crypto=true
                ;;
            --without-crypto)
                _include_crypto=false
                ;;
            --with-tui)
                _include_tui=true
                ;;
            --without-tui)
                _include_tui=false
                ;;
            # Advanced package groups
            --with-advanced-fs)
                _include_advanced_fs=true
                ;;
            --without-advanced-fs)
                _include_advanced_fs=false
                ;;
            --with-disk-diag)
                _include_disk_diag=true
                ;;
            --without-disk-diag)
                _include_disk_diag=false
                ;;
            --with-network-diag)
                _include_network_diag=true
                ;;
            --without-network-diag)
                _include_network_diag=false
                ;;
            --with-system-tools)
                _include_system_tools=true
                ;;
            --without-system-tools)
                _include_system_tools=false
                ;;
            --with-data-recovery)
                _include_data_recovery=true
                ;;
            --without-data-recovery)
                _include_data_recovery=false
                ;;
            --with-boot-repair)
                _include_boot_repair=true
                ;;
            --without-boot-repair)
                _include_boot_repair=false
                ;;
            --with-editors)
                _include_editors=true
                ;;
            --without-editors)
                _include_editors=false
                ;;
            --with-security)
                _include_security=true
                ;;
            --without-security)
                _include_security=false
                ;;
            --with-all-advanced)
                _include_advanced_fs=true
                _include_disk_diag=true
                _include_network_diag=true
                _include_system_tools=true
                _include_data_recovery=true
                _include_boot_repair=true
                _include_editors=true
                _include_security=true
                ;;
            --without-all-advanced)
                _include_advanced_fs=false
                _include_disk_diag=false
                _include_network_diag=false
                _include_system_tools=false
                _include_data_recovery=false
                _include_boot_repair=false
                _include_editors=false
                _include_security=false
                ;;
            --dry-run)
                # Export immediately instead of waiting for the apply_changes check
                export DRY_RUN=true
                echo "DEBUG: Dry run mode activated - will parse arguments but not perform actual build" >&2
                _temp_log "Dry run mode activated - will show configuration but not run actual build"
                ;;
        esac
    done
    
    # Apply the changes to environment variables if requested
    if [ "$apply_changes" = "true" ]; then
        # Create a flag variable to track if we used a profile function
        local used_profile_function=false
        
        # Check for special case handling
        if [[ "$build_args" == *"--minimal"* ]] && type -t set_minimal_profile &>/dev/null; then
            # We already applied the minimal profile earlier, don't override it
            used_profile_function=true
            echo "DEBUG: Skipping variable export since set_minimal_profile was used" >&2
        elif [[ "$build_args" == *"--full"* ]] && type -t set_full_profile &>/dev/null; then
            # We already applied the full profile earlier, don't override it
            used_profile_function=true
            echo "DEBUG: Skipping variable export since set_full_profile was used" >&2
        fi
        
        # Only set from _include_* variables if we didn't use a profile function
        if [ "$used_profile_function" != "true" ]; then
            echo "DEBUG: Exporting variables from _include_* local variables" >&2
            export INCLUDE_MINIMAL_KERNEL="$_include_minimal_kernel"
            export INCLUDE_ZFS="$_include_zfs"
            export INCLUDE_BTRFS="$_include_btrfs"
            export INCLUDE_RECOVERY_TOOLS="$_include_recovery_tools"
            export INCLUDE_NETWORK_TOOLS="$_include_network_tools"
            export INCLUDE_CRYPTO="$_include_crypto"
            export INCLUDE_TUI="$_include_tui"
            export INCLUDE_ADVANCED_FS="$_include_advanced_fs"
            export INCLUDE_DISK_DIAG="$_include_disk_diag"
            export INCLUDE_NETWORK_DIAG="$_include_network_diag"
            export INCLUDE_SYSTEM_TOOLS="$_include_system_tools"
            export INCLUDE_DATA_RECOVERY="$_include_data_recovery"
            export INCLUDE_BOOT_REPAIR="$_include_boot_repair"
            export INCLUDE_EDITORS="$_include_editors"
            export INCLUDE_SECURITY="$_include_security"
        fi
        
        # Print summary info in any case
        _temp_log "Applied feature flags from build arguments"
    fi
    
    # Return a summary string for logging
    local summary="INCLUDE_MINIMAL_KERNEL=$_include_minimal_kernel "
    summary+="INCLUDE_ZFS=$_include_zfs "
    summary+="INCLUDE_NETWORK_TOOLS=$_include_network_tools "
    summary+="INCLUDE_CRYPTO=$_include_crypto"
    
    echo "$summary"
}

# Export all functions
export -f is_github_actions
export -f is_docker_container
export -f is_restricted_environment
export -f needs_extraction
export -f mark_extraction_complete
export -f fix_kernel_permissions
export -f print_environment_info
export -f fix_script_permissions
export -f ensure_directory
export -f parse_build_flags
# Handle kernel permissions in a CI-friendly way
# Some CI environments don't allow chmod on certain files
handle_kernel_permissions() {
    local kernel_dir="$1"
    
    if [ ! -d "$kernel_dir" ]; then
        log "WARNING" "Kernel directory not found: $kernel_dir"
        return 1
    fi
    
    log "INFO" "Ensuring kernel build scripts are executable"
    
    # Make Makefile executable
    if [ -f "$kernel_dir/Makefile" ]; then
        chmod +x "$kernel_dir/Makefile" 2>/dev/null || log "WARNING" "Could not change Makefile permissions, continuing anyway"
    fi
    
    # Make shell scripts executable (only necessary permissions, not directory creation)
    if [ -d "$kernel_dir/scripts" ]; then
        find "$kernel_dir/scripts" -name "*.sh" -type f -exec chmod +x {} \; 2>/dev/null || log "WARNING" "Could not change script permissions, continuing anyway"
    fi
    
    log "INFO" "Script permissions set"
    return 0
}

export -f configure_alpine
export -f setup_kernel_config
export -f setup_zfs
export -f get_optimal_threads
export -f get_memory_optimized_cflags
export -f print_section
# print_banner is now imported from 80_common.sh
export -f docker_handle_extraction
export -f prepare_alpine_minirootfs
export -f prepare_chroot
export -f cleanup_chroot
export -f run_in_chroot
export -f get_latest_alpine_version
export -f get_alpine_minirootfs_url
export -f handle_kernel_permissions