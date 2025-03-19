#!/bin/bash
# Docker entrypoint script for OneFileLinux builder
set -e

# Fixed uid/gid or take from environment
USER_ID=${USER_ID:-1000}
GROUP_ID=${GROUP_ID:-1000}

echo "Starting with UID: $USER_ID, GID: $GROUP_ID"

# Make sure we have a /tmp directory with proper permissions
mkdir -p /tmp
chmod 1777 /tmp

# Create a fresh work directory with all permissions for extraction
TEMP_EXTRACT_DIR="/onefilelinux/extract_temp"
mkdir -p "$TEMP_EXTRACT_DIR"
chmod 777 "$TEMP_EXTRACT_DIR"

# Make sure we have build directories
mkdir -p /onefilelinux/build
mkdir -p /onefilelinux/output
mkdir -p /onefilelinux/.buildcache

# Ensure proper permissions for output directory
chmod 777 /onefilelinux/output

# Get build directory absolute path
BUILD_DIR=$(cd /onefilelinux/build && pwd)

# Update the builder user's uid/gid if needed
if [ "$USER_ID" != "1000" ] || [ "$GROUP_ID" != "1000" ]; then
    echo "Updating builder user to match host UID/GID"
    
    # First, set alpine-minirootfs directory to root if it exists
    if [ -d "$BUILD_DIR/alpine-minirootfs" ]; then
        chown -R root:root "$BUILD_DIR/alpine-minirootfs" || true
    fi
    
    # Update user
    deluser builder
    addgroup -g $GROUP_ID builder
    adduser -D -u $USER_ID -G builder builder
    echo "builder ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/builder
fi

# Set proper ownership for build directories, excluding special directories
log_info() {
    echo "[INFO] $1"
}

log_warn() {
    echo "[WARN] $1"
}

log_info "Setting up build environment..."
# Instead of changing ownership of all mounted volumes (which is slow),
# we'll change Docker's approach to use the host user's UID/GID directly

# Ensure runtime directories exist and have correct permissions
mkdir -p /onefilelinux/build /onefilelinux/output /onefilelinux/.buildcache
chmod 755 /onefilelinux/build /onefilelinux/output /onefilelinux/.buildcache

# Set ownership of the .buildcache directory which is a Docker volume with persistence
# This is necessary for proper ccache operation
chown -R builder:builder /onefilelinux/.buildcache 2>/dev/null || true

# For mounted host volumes (build and output), we skip ownership changes
# This is much faster and works better with bind mounts from the host
log_info "Using host's ownership for mounted volumes"

# Ensure specific directories exist with correct permissions
if [ -d "/onefilelinux/build/alpine-minirootfs" ]; then
    log_info "Setting up special directories in alpine-minirootfs..."
    
    # Create and set permissions for key directories
    if [ -d "/onefilelinux/build/alpine-minirootfs/proc" ]; then
        chmod 555 "/onefilelinux/build/alpine-minirootfs/proc" 2>/dev/null || log_warn "Could not set permissions on /proc (this is normal in Docker)"
    fi
    
    if [ -d "/onefilelinux/build/alpine-minirootfs/var/empty" ]; then
        chmod 555 "/onefilelinux/build/alpine-minirootfs/var/empty" 2>/dev/null || log_warn "Could not set permissions on /var/empty (this is normal in Docker)"
    fi
fi

# Add environment variable to indicate we're in a Docker environment
export IN_DOCKER_CONTAINER=true

# Function to fix kernel permissions (called before build)
fix_kernel_permissions() {
    log_info "Checking for kernel source directory permissions"
    
    # Check if linux directory exists
    if [ -d "/onefilelinux/build/linux" ]; then
        log_info "Fixing permissions for kernel source directory"
        
        # Find problematic directories with incorrect permissions
        find /onefilelinux/build/linux -type d -not -perm -u+rwx -exec chmod u+rwx {} \; 2>/dev/null || true
        
        # Find problematic files with incorrect permissions
        find /onefilelinux/build/linux -type f -not -perm -u+rw -exec chmod u+rw {} \; 2>/dev/null || true
        
        # Specifically handle common problem areas
        chmod -R u+rwX /onefilelinux/build/linux/include 2>/dev/null || true
        chmod -R u+rwX /onefilelinux/build/linux/arch 2>/dev/null || true
        chmod -R u+rwX /onefilelinux/build/linux/scripts 2>/dev/null || true
        
        # Make sure scripts are executable
        find /onefilelinux/build/linux/scripts -name "*.sh" -exec chmod +x {} \; 2>/dev/null || true
        
        log_info "Kernel source permissions fixed"
    else
        log_info "Kernel source directory not found, skipping permission fix"
    fi
    
    # Make sure all library scripts are executable
    log_info "Making all library scripts executable"
        
    # Make core library scripts executable
    find /onefilelinux/build -name "8*_*.sh" -exec chmod +x {} \; 2>/dev/null || true
}

# Set up performance optimizations
log_info "Setting up build performance optimizations"

# Configure ccache for better performance
export CCACHE_DIR=/onefilelinux/.buildcache/ccache
export PATH=/usr/lib/ccache:$PATH

# Check if we have enough memory for parallel builds
if [ -f "/proc/meminfo" ]; then
    available_memory_kb=$(grep MemAvailable /proc/meminfo 2>/dev/null | awk '{print $2}' || echo 0)
    if [ "$available_memory_kb" -gt 0 ]; then
        available_memory_gb=$(awk "BEGIN {printf \"%.1f\", $available_memory_kb/1024/1024}")
        log_info "Available memory: ${available_memory_gb}GB"
        
        # Calculate optimal number of jobs based on memory
        # Each job needs about 2GB for kernel compilation
        optimal_jobs=$(awk "BEGIN {print int($available_memory_kb/1024/1024/2)}")
        # Ensure at least 1 job and get the minimum of this and the number of CPUs
        cpu_count=$(nproc 2>/dev/null || echo 2)
        optimal_jobs=$(( optimal_jobs < 1 ? 1 : optimal_jobs ))
        optimal_jobs=$(( optimal_jobs > cpu_count ? cpu_count : optimal_jobs ))
        
        log_info "Automatically using $optimal_jobs parallel jobs based on available memory"
        export BUILD_JOBS=$optimal_jobs
        
        # Add to BUILD_ARGS if not already specified
        if [ -n "$BUILD_ARGS" ] && [[ "$BUILD_ARGS" != *"--jobs="* ]]; then
            BUILD_ARGS="$BUILD_ARGS --jobs=$optimal_jobs"
        fi
    fi
fi

# Initialize ccache with optimal settings
ccache -M 5G 2>/dev/null || true
ccache -o compression=true 2>/dev/null || true
ccache -o compression_level=6 2>/dev/null || true
ccache -z 2>/dev/null || true

# Define a lightweight extraction function for bootstrapping
# This is used before we can access our library functions
bootstrap_extraction() {
    local src_file="$1"
    local dest_dir="$2"
    
    echo "Bootstrapping extraction of $src_file to $dest_dir"
    
    # Create destination directory first
    mkdir -p "$dest_dir"
    
    # Use temp directory for intermediate extraction
    mkdir -p "$TEMP_EXTRACT_DIR"
    
    # Extract as root first to a temp location
    if [[ "$src_file" == *.tar.gz ]]; then
        tar -xzf "$src_file" -C "$TEMP_EXTRACT_DIR"
    elif [[ "$src_file" == *.tar.xz ]]; then
        # For .tar.xz files, must decompress and extract separately
        xz -dc "$src_file" | tar -x -C "$TEMP_EXTRACT_DIR"
    fi
    
    # Copy files to final location
    cp -a "$TEMP_EXTRACT_DIR"/* "$dest_dir"/ || true
    
    # Clean up
    rm -rf "$TEMP_EXTRACT_DIR"/*
    
    # Set permissions
    chown -R builder:builder "$dest_dir"
    
    echo "Bootstrapping extraction completed to $dest_dir"
    return 0
}

# Export the function for bootstrapping
export -f bootstrap_extraction

# Set working directory
cd /onefilelinux/build

# Define function to run build
run_build() {
    # Fix kernel permissions to avoid build errors
    fix_kernel_permissions
    
    # Check for the core library scripts
    if [ -f "80_common.sh" ] && [ -f "81_error_handling.sh" ] && [ -f "82_build_helper.sh" ] && [ -f "83_config_helper.sh" ] && [ -f "84_build_core.sh" ]; then
        # Use the library-based build system for consistent builds
        echo "Using library-based build system"
        
        # Make all library scripts executable
        chmod +x 80_common.sh
        chmod +x 81_error_handling.sh
        chmod +x 82_build_helper.sh
        chmod +x 83_config_helper.sh
        chmod +x 84_build_core.sh
        
        # Make build scripts executable
        chmod +x 01_get.sh
        chmod +x 02_chrootandinstall.sh
        chmod +x 03_conf.sh
        chmod +x 04_build.sh
        
        # No pre-initialization needed - each script will properly load libraries
        # This line was previously causing double initialization of error handling
        
        # Make sure the build script exists and is executable
        if [ -f "04_build.sh" ]; then
            chmod +x 04_build.sh
            echo "Found build script: 04_build.sh"
        else
            echo "WARNING: Build script not found in container"
            echo "Searching for the script in the build directory..."
            
            # Find the script in the mounted volumes
            SCRIPT_PATH=$(find /onefilelinux -name "04_build.sh" -type f 2>/dev/null | head -n 1)
            
            if [ -n "$SCRIPT_PATH" ]; then
                echo "Found script at: $SCRIPT_PATH"
                echo "Creating symlink to make script accessible"
                ln -sf "$SCRIPT_PATH" ./04_build.sh
                chmod +x 04_build.sh
            else
                echo "ERROR: Critical file not found: 04_build.sh"
                echo "Please ensure the build directory is correctly mounted"
            fi
        fi
        
        # Use the unified build script for complete timing information
        # Make sure the unified build script exists and is executable
        if [ -f "build.sh" ]; then
            chmod +x build.sh
        else
            echo "Creating build.sh symlink"
            # Find the script in the mounted volumes
            SCRIPT_PATH=$(find /onefilelinux -name "build.sh" -type f 2>/dev/null | head -n 1)
            
            if [ -n "$SCRIPT_PATH" ]; then
                echo "Found script at: $SCRIPT_PATH"
                ln -sf "$SCRIPT_PATH" ./build.sh
                chmod +x build.sh
            else
                echo "ERROR: Unified build script not found: build.sh"
                echo "Please ensure the build directory contains build.sh"
                return 1
            fi
        fi
        
        # Add common options for better performance
        if [ -n "$BUILD_ARGS" ]; then
            # Add our performance options if not already included
            if [[ "$BUILD_ARGS" != *"--use-cache"* ]]; then
                BUILD_ARGS="$BUILD_ARGS --use-cache"
            fi
            if [[ "$BUILD_ARGS" != *"--use-swap"* && "$BUILD_ARGS" != *"--no-swap"* ]]; then
                BUILD_ARGS="$BUILD_ARGS --use-swap"
            fi
            
            # Try a more explicit step-by-step approach in Docker environment
            echo "Running build steps sequentially for Docker environment:"
            
            # Display ccache stats before build
            echo "CCache statistics before build:"
            ccache -s
            
            # Parse build arguments early to set feature flags for all scripts
            echo "Loading build helper library to parse build arguments"
            source "./82_build_helper.sh"
            echo "Parsing build flags from arguments: $BUILD_ARGS"
            parse_build_flags "$BUILD_ARGS" true
            
            # If --minimal flag was detected, also set BUILD_TYPE for 04_build.sh compatibility
            if [ "${INCLUDE_MINIMAL_KERNEL:-false}" = "true" ]; then
                echo "Setting BUILD_TYPE=minimal based on --minimal flag detection"
                export BUILD_TYPE="minimal"
                
                # Set active build profile if the function is available
                if type -t set_active_build_profile &>/dev/null; then
                    echo "Setting active build profile to 'minimal'"
                    set_active_build_profile "minimal"
                else
                    # Create active_profile.txt directly
                    echo "minimal" > active_profile.txt
                    echo "Created active_profile.txt with 'minimal' profile"
                fi
            fi

            # Log the configuration that will be used
            echo "Using build configuration:"
            echo "- ZFS support: ${INCLUDE_ZFS:-true}"
            echo "- Network tools: ${INCLUDE_NETWORK_TOOLS:-true}"
            echo "- Crypto support: ${INCLUDE_CRYPTO:-true}"
            echo "- Minimal kernel: ${INCLUDE_MINIMAL_KERNEL:-false}"
            
            # Run each step explicitly to ensure proper error handling
            echo "Step 1: Running 01_get.sh to download components"
            ./01_get.sh
            
            echo "Step 2: Running 02_chrootandinstall.sh to prepare the environment"
            ./02_chrootandinstall.sh
            
            echo "Step 3: Running 03_conf.sh for configuration"
            ./03_conf.sh
            
            echo "Step 4: Running 04_build.sh for final build with args: $BUILD_ARGS"
            
            # Set flag to finalize timing log in the last script
            export FINALIZE_TIMING_LOG=true
            
            # Pass the build arguments directly to 04_build.sh
            ./04_build.sh $BUILD_ARGS
            
            # Display ccache stats after build
            echo "CCache statistics after build:"
            ccache -s
        else
            echo "Running build steps sequentially for Docker environment (default options):"
            echo "CCache statistics before build:"
            ccache -s
            
            # Run each step explicitly to ensure proper error handling
            echo "Step 1: Running 01_get.sh to download components"
            ./01_get.sh
            
            echo "Step 2: Running 02_chrootandinstall.sh to prepare the environment"
            ./02_chrootandinstall.sh
            
            echo "Step 3: Running 03_conf.sh for configuration"
            ./03_conf.sh
            
            echo "Step 4: Running 04_build.sh for final build with default args"
            
            # Set flag to finalize timing log in the last script
            export FINALIZE_TIMING_LOG=true
            
            # Pass default build arguments directly to 04_build.sh
            ./04_build.sh --use-cache --use-swap
            
            echo "CCache statistics after build:"
            ccache -s
        fi
    else
        # Required scripts are missing
        echo "ERROR: Core library scripts not found"
        echo "Please ensure the build directory contains all required scripts"
        
        # Handle special alpine extraction issues
        if [ -f "alpine-minirootfs-3.21.3-x86_64.tar.gz" ]; then
            echo "Pre-extracting Alpine minirootfs as root"
            # Remove existing directory if it exists
            if [ -d "alpine-minirootfs" ]; then
                rm -rf alpine-minirootfs
            fi
            
            # Create directory with proper permissions
            mkdir -p alpine-minirootfs
            
            # Use bootstrap extraction first
            bootstrap_extraction "alpine-minirootfs-3.21.3-x86_64.tar.gz" "alpine-minirootfs"
            
            # Mark extraction as complete
            touch "alpine-minirootfs/.extraction_complete"
            
            echo "Set proper permissions on Alpine minirootfs"
        fi
        
        # Attempt to run the build script if it exists
        if [ -f "04_build.sh" ]; then
            chmod +x 04_build.sh
            if [ -n "$BUILD_ARGS" ]; then
                echo "Running: ./04_build.sh $BUILD_ARGS"
                echo "WARNING: Using direct build.sh approach is deprecated. Prefer library-based build system."
                ./04_build.sh $BUILD_ARGS
            else
                echo "Running: ./04_build.sh"
                echo "WARNING: Using direct build.sh approach is deprecated. Prefer library-based build system."
                ./04_build.sh
            fi
        else
            echo "ERROR: Build script 04_build.sh not found"
            echo "Please ensure the build directory is correctly mounted"
            return 1
        fi
    fi
}

# Check if we should run as root or as the builder user
if [ "${RUN_AS_ROOT:-false}" = "true" ]; then
    echo "Running as root user (RUN_AS_ROOT=true)"
    if [ $# -eq 0 ]; then
        # Default command if none provided
        cd /onefilelinux/build && run_build
    else
        # Run whatever command was passed
        "$@"
    fi
else
    echo "Running as builder user"
    if [ $# -eq 0 ]; then
        # Default command if none provided
        exec su-exec builder bash -c "cd /onefilelinux/build && run_build"
    else
        # Run whatever command was passed
        exec su-exec builder "$@"
    fi
fi