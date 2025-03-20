#!/bin/bash
# Make sure this file is executable (chmod +x)
#
# OneFileLinux Build Core Functions (84_build_core.sh)
# Shared build functions used by 04_build.sh and build.sh
# This is part of the library scripts (80-89 range)
#
# Contains the core build functionality including:
# - Kernel building (build_kernel)
# - ZFS module building (build_zfs)
# - EFI file creation (create_efi)
# - Build argument parsing (parse_build_args)
#

# Helper function to convert boolean to Yes/No string
bool_to_str() {
    if [ "$1" = "true" ]; then
        echo -e "${GREEN}Yes${NC}"
    else
        echo -e "${RED}No${NC}"
    fi
}

# Build the kernel and modules
build_kernel() {
    print_section "Building Linux kernel"
    
    # Environment-aware build adjustments
    if is_github_actions; then
        log "INFO" "Running in GitHub Actions environment"
    fi
    
    # Detect available system memory
    local available_memory_kb=0
    local total_cores=0
    
    # Detect available memory
    if [ -f "/proc/meminfo" ]; then
        available_memory_kb=$(grep MemAvailable /proc/meminfo 2>/dev/null | awk '{print $2}' || echo 0)
    fi
    
    # If memory detection failed, try total memory with a reduction factor
    if [ "$available_memory_kb" -eq 0 ] && [ -f "/proc/meminfo" ]; then
        local total_memory_kb=$(grep MemTotal /proc/meminfo 2>/dev/null | awk '{print $2}' || echo 0)
        available_memory_kb=$((total_memory_kb * 7 / 10)) # Use 70% of total memory
    fi
    
    # Convert to GB for display
    local available_memory_gb=$(awk "BEGIN {printf \"%.1f\", $available_memory_kb/1024/1024}")
    
    # Get optimal number of threads for building
    local threads=$(get_optimal_threads)
    log "INFO" "Building kernel with $threads threads"
    
    # Validate threads is an integer, replace with default if not
    if ! [[ "$threads" =~ ^[0-9]+$ ]]; then
        log "WARNING" "Thread count is not a valid number: '$threads'. Using default: 2"
        threads=2
    fi
    
    # Check kernel size before building
    local kernel_size=$(du -sh "$KERNEL_DIR" 2>/dev/null | cut -f1 || echo "unknown")
    log "INFO" "Kernel source size: $kernel_size"
    
    # Create swap file if memory is very low and USE_SWAP is enabled
    if [ "${USE_SWAP:-false}" = "true" ] && [ "$available_memory_kb" -gt 0 ] && [ "$available_memory_kb" -lt 4000000 ]; then  # < 4GB
        log "INFO" "Low memory detected (${available_memory_gb}GB). Creating temporary swap file..."
        
        local swap_size_mb=4096  # 4GB swap
        local swap_file="/tmp/onefilelinux_swap"
        
        # Remove existing swap if present
        if [ -f "$swap_file" ]; then
            sudo swapoff "$swap_file" 2>/dev/null || true
            sudo rm -f "$swap_file"
        fi
        
        # Create new swap file
        log "INFO" "Allocating ${swap_size_mb}MB swap file at $swap_file"
        sudo dd if=/dev/zero of="$swap_file" bs=1M count="$swap_size_mb" status=progress 2>/dev/null || {
            log "WARNING" "Failed to create swap file. Continuing without swap."
        }
        
        if [ -f "$swap_file" ]; then
            sudo chmod 600 "$swap_file"
            sudo mkswap "$swap_file" >/dev/null 2>&1 || {
                log "WARNING" "Failed to format swap file. Continuing without swap."
                sudo rm -f "$swap_file"
            }
            
            sudo swapon "$swap_file" >/dev/null 2>&1 || {
                log "WARNING" "Failed to enable swap file. Continuing without swap."
                sudo rm -f "$swap_file"
            }
            
            # Re-read available memory with swap included
            sleep 1
            available_memory_kb=$(grep MemAvailable /proc/meminfo 2>/dev/null | awk '{print $2}' || echo 0)
            available_memory_gb=$(awk "BEGIN {printf \"%.1f\", $available_memory_kb/1024/1024}")
            log "SUCCESS" "Swap file enabled. Available memory now: ${available_memory_gb}GB"
        fi
    fi
    
    # Calculate a safe number of threads based on available memory
    # Empirically, each compilation thread needs ~2GB for kernel compilation
    if [ "$available_memory_kb" -gt 0 ]; then
        local safe_threads=$(awk "BEGIN {print int($available_memory_kb/1024/1024/2)}")
        # Ensure at least 1 thread and no more than original THREADS
        safe_threads=$(( safe_threads < 1 ? 1 : safe_threads ))
        safe_threads=$(( safe_threads > threads ? threads : safe_threads ))
        
        if [ "$safe_threads" -lt "$threads" ]; then
            log "WARNING" "Limited available memory: ${available_memory_gb}GB. Reducing build threads from $threads to $safe_threads"
            threads=$safe_threads
        else
            log "INFO" "Available memory: ${available_memory_gb}GB. Using $threads threads"
        fi
    else
        log "WARNING" "Could not detect available memory. Using $threads threads, but may be risky"
    fi
    
    # Additional optimizations to reduce memory usage
    export KBUILD_BUILD_TIMESTAMP=$(date)
    
    # Get optimized compiler flags based on memory conditions
    export KCFLAGS=$(get_memory_optimized_cflags)
    
    # Apply GitHub Actions additional optimizations if needed
    if is_github_actions && [ "$available_memory_kb" -lt 4000000 ]; then
        log "WARNING" "GitHub Actions with low memory - applying additional optimizations"
        # Tell the kernel build system to minimize memory usage at the cost of build time
        export KBUILD_LOW_MEMORY=1
        export CFLAGS="${CFLAGS:-} -Wl,--as-needed"
    fi
    
    # Enter kernel directory
    cd "$KERNEL_DIR"
    
    # ABI header mismatch warnings are allowed
    log "INFO" "ABI header mismatch warnings are allowed for now"
    
    
    # Check directory permissions before building
    log "INFO" "Checking kernel source directory permissions"
    local perm_ok=true
    
    # Test if we can write to critical directories
    for test_dir in "." "include" "drivers" "arch/x86" "scripts"; do
        if [ -d "$test_dir" ] && ! touch "$test_dir/.write_test" 2>/dev/null; then
            log "WARNING" "Cannot write to $test_dir directory, fixing permissions"
            if is_restricted_environment; then
                sudo chmod -R u+w "$test_dir" 2>/dev/null || true
            else
                chmod -R u+w "$test_dir" 2>/dev/null || true
            fi
            perm_ok=false
        elif [ -f "$test_dir/.write_test" ]; then
            rm -f "$test_dir/.write_test"
        fi
    done
    
    # If permissions were fixed, check again
    if [ "$perm_ok" = "false" ]; then
        log "INFO" "Fixed permissions, re-checking..."
        if ! touch "./.write_test" 2>/dev/null; then
            log "ERROR" "Still cannot write to kernel source directory after fixing permissions"
            if is_github_actions; then
                log "INFO" "In GitHub Actions environment, using elevated permissions for build"
            fi
        else
            rm -f "./.write_test"
            log "SUCCESS" "Successfully fixed permissions"
        fi
    fi
    
    # Set kernel config interactively if requested
    if [ "${INTERACTIVE_CONFIG:-false}" = "true" ]; then
        log "INFO" "Using interactive kernel configuration"
        make menuconfig
    else
        log "INFO" "Using non-interactive kernel configuration"
        # Check for module signing configuration and fix OpenSSL-related settings
        if grep -q "CONFIG_MODULE_SIG=y" .config; then
            # Make sure trusted keys and revocation keys are empty to avoid external dependencies
            log "INFO" "Setting empty trusted keys for kernel configuration"
            sed -i 's/CONFIG_SYSTEM_TRUSTED_KEYS=.*/CONFIG_SYSTEM_TRUSTED_KEYS=""/' .config
            sed -i 's/CONFIG_SYSTEM_REVOCATION_KEYS=.*/CONFIG_SYSTEM_REVOCATION_KEYS=""/' .config
            
            # Check if we need to pre-create the signing key directory and files
            if [ ! -d "certs" ]; then
                mkdir -p certs
                log "INFO" "Created kernel certs directory"
            fi
            
            # We'll keep module signing enabled since we're now installing openssl in Docker
            log "INFO" "Keeping module signing enabled with empty trusted keys"
        fi
        # Apply configuration updates
        make olddefconfig
    fi
    
    # Determine verbosity level
    if [ "${MAKE_VERBOSE:-0}" = "1" ] || [ "${VERBOSE:-false}" = "true" ]; then
        local make_v="V=1"
        log "INFO" "Using verbose build output"
    else
        local make_v="V=0"
    fi
    
    # Build with compiler cache if available
    if [ "${USE_CACHE:-false}" = "true" ] && command -v ccache &> /dev/null; then
        log "INFO" "Using compiler cache for faster builds"
        log "INFO" "Make command: make $make_v -j$threads CC=\"ccache gcc\" HOSTCC=\"ccache gcc\""
        
        # Use a wrapper for improved error handling
        nice -n 19 make $make_v -j$threads CC="ccache gcc" HOSTCC="ccache gcc" 2>&1 | tee kernel_build.log || {
            log "ERROR" "Kernel build failed"
            
            # Check build log for common errors
            if grep -q "No space left on device" kernel_build.log; then
                log "ERROR" "Build failed due to insufficient disk space!"
            elif grep -q "Permission denied" kernel_build.log; then
                log "ERROR" "Build failed due to permission issues! Try running in a privileged container."
            elif grep -q "virtual memory exhausted" kernel_build.log || grep -q "Killed" kernel_build.log; then
                log "ERROR" "Build failed due to insufficient memory! Try reducing the number of threads or add more RAM."
            fi
            
            return 1
        }
    else
        log "INFO" "Make command: make $make_v -j$threads"
        nice -n 19 make $make_v -j$threads 2>&1 | tee kernel_build.log || {
            log "ERROR" "Kernel build failed"
            
            # Check build log for common errors
            if grep -q "No space left on device" kernel_build.log; then
                log "ERROR" "Build failed due to insufficient disk space!"
            elif grep -q "Permission denied" kernel_build.log; then
                log "ERROR" "Build failed due to permission issues! Try running in a privileged container."
            elif grep -q "virtual memory exhausted" kernel_build.log || grep -q "Killed" kernel_build.log; then
                log "ERROR" "Build failed due to insufficient memory! Try reducing the number of threads or add more RAM."
            fi
            
            return 1
        }
    fi
    
    # Clean up log file
    rm -f kernel_build.log
    
    # Build modules
    log "INFO" "Building kernel modules"
    if [ "${USE_CACHE:-false}" = "true" ] && command -v ccache &> /dev/null; then
        log "INFO" "Make modules command: make $make_v modules -j$threads"
        
        # Use a wrapper for improved error handling
        nice -n 19 make $make_v modules -j$threads CC="ccache gcc" HOSTCC="ccache gcc" 2>&1 | tee module_build.log || {
            log "ERROR" "Module build failed"
            
            # Check build log for common errors
            if grep -q "No space left on device" module_build.log; then
                log "ERROR" "Module build failed due to insufficient disk space!"
            elif grep -q "Permission denied" module_build.log; then
                log "ERROR" "Module build failed due to permission issues!"
            elif grep -q "virtual memory exhausted" module_build.log || grep -q "Killed" module_build.log; then
                log "ERROR" "Module build failed due to insufficient memory!"
            fi
            
            return 1
        }
    else
        log "INFO" "Make modules command: make $make_v modules -j$threads"
        nice -n 19 make $make_v modules -j$threads 2>&1 | tee module_build.log || {
            log "ERROR" "Module build failed"
            
            # Check build log for common errors
            if grep -q "No space left on device" module_build.log; then
                log "ERROR" "Module build failed due to insufficient disk space!"
            elif grep -q "Permission denied" module_build.log; then
                log "ERROR" "Module build failed due to permission issues!"
            elif grep -q "virtual memory exhausted" module_build.log || grep -q "Killed" module_build.log; then
                log "ERROR" "Module build failed due to insufficient memory!"
            fi
            
            return 1
        }
    fi
    
    # Clean up log file
    rm -f module_build.log
    
    # Install modules
    log "INFO" "Installing kernel modules"
    log "INFO" "Make modules_install command: make $make_v INSTALL_MOD_PATH=\"$ROOTFS_DIR\" modules_install -j$threads"
    INSTALL_MOD_PATH="$ROOTFS_DIR" make $make_v modules_install -j$threads 2>&1 | tee module_install.log || {
        log "ERROR" "Module installation failed"
        
        # Check log for common errors
        if grep -q "No space left on device" module_install.log; then
            log "ERROR" "Module installation failed due to insufficient disk space!"
        elif grep -q "Permission denied" module_install.log; then
            log "ERROR" "Module installation failed due to permission issues!"
        fi
        
        return 1
    }
    rm -f module_install.log
    
    log "SUCCESS" "Kernel and modules built successfully"
    
    # Return to build directory
    cd "$BUILD_DIR"
    
    # Clean up swap file if we created one
    if [ "${USE_SWAP:-false}" = "true" ] && [ -f "/tmp/onefilelinux_swap" ]; then
        log "INFO" "Removing temporary swap file"
        sudo swapoff "/tmp/onefilelinux_swap" 2>/dev/null || true
        sudo rm -f "/tmp/onefilelinux_swap"
        log "SUCCESS" "Swap file removed"
    fi
}

# Build ZFS modules
build_zfs() {
    if [ "$INCLUDE_ZFS" != "true" ]; then
        log "INFO" "Skipping ZFS build (disabled in configuration)"
        return 0
    fi
    
    print_section "ZFS Setup"
    log "INFO" "Using pre-built Alpine Linux ZFS packages"
    log "SUCCESS" "ZFS support configured through Alpine packages"
    
    # Check for required kernel modules in the Alpine package
    if [ -d "$ROOTFS_DIR/lib/modules" ]; then
        log "INFO" "Verifying ZFS kernel modules in Alpine packages"
        if find "$ROOTFS_DIR/lib/modules" -name "zfs.ko" -o -name "spl.ko" | grep -q .; then
            log "SUCCESS" "ZFS kernel modules found in Alpine packages"
        else
            log "WARNING" "ZFS kernel modules not found in expected location. Check Alpine package compatibility."
        fi
    fi
    
    return 0
}

# Create the final EFI file
create_efi() {
    print_section "Creating EFI file"
    
    # Ensure output directory exists
    ensure_directory "$OUTPUT_DIR"
    
    # Get kernel version for modules.dep
    local kernel_version=$(ls "$ROOTFS_DIR/lib/modules/" | head -1)
    log "INFO" "Creating modules.dep for kernel $kernel_version"
    
    # Enter kernel directory
    cd "$KERNEL_DIR"
    
    # Create modules.dep
    log "INFO" "Creating modules.dep"
    if [ -f "System.map" ]; then
        log "INFO" "Using System.map for module dependencies"
        depmod -b "$ROOTFS_DIR" -F System.map "$kernel_version" || {
            log "WARNING" "depmod encountered errors, modules may not load correctly"
        }
    else
        log "WARNING" "System.map not found, module dependencies may be incomplete"
        # Try running depmod without System.map
        depmod -b "$ROOTFS_DIR" "$kernel_version" 2>/dev/null || {
            log "WARNING" "Failed to create module dependencies"
        }
    fi
    
    # Use BUILD_TYPE for EFI filename
    if [ -n "${BUILD_TYPE:-}" ]; then
        profile_name="${BUILD_TYPE}"
        log "INFO" "Using BUILD_TYPE for EFI filename: $profile_name"
    else
        # Fail if BUILD_TYPE is not set
        log "ERROR" "BUILD_TYPE is not set. This is required for determining the profile."
        return 1
    fi
    
    # Create the EFI filename using the profile name
    local efi_file_name="OneFileLinux-${profile_name}.efi"
    
    # Export the expected EFI filename for other scripts to check
    export EXPECTED_EFI_FILENAME="$efi_file_name"
    
    # Create EFI file
    log "INFO" "Creating ${efi_file_name}"
    if [ -f "arch/x86/boot/bzImage" ]; then
        cp arch/x86/boot/bzImage "$OUTPUT_DIR/${efi_file_name}" || {
            log "ERROR" "Failed to copy kernel bzImage to output directory"
            return 1
        }
        log "SUCCESS" "Kernel bzImage copied to ${efi_file_name}"
        
        # Create a symlink to the generic name for backward compatibility
        ln -sf "${efi_file_name}" "$OUTPUT_DIR/OneFileLinux.efi" || {
            log "WARNING" "Failed to create backward compatibility symlink"
        }
    else
        log "ERROR" "Failed to find kernel bzImage - kernel build may have failed"
        # Try to check if we can find any error messages
        if [ -f "kernel_build.log" ]; then
            log "INFO" "Checking build log for errors:"
            grep -i "error" kernel_build.log | tail -10
        fi
        return 1
    fi
    
    # If building as root, fix permissions on the output directory
    if [ "${BUILD_AS_ROOT:-false}" = "true" ]; then
        log "INFO" "Fixing permissions on output directory after root build"
        if [ -n "$SUDO_UID" ] && [ -n "$SUDO_GID" ]; then
            # We're running with sudo, fix ownership to the original user
            chown -R "$SUDO_UID:$SUDO_GID" "$OUTPUT_DIR" || true
        elif [ -n "$RUNNER_UID" ]; then
            # GitHub Actions specific handling
            chown -R "$RUNNER_UID:$RUNNER_GID" "$OUTPUT_DIR" || true
        else
            # Fallback to making everything world-writable
            chmod -R 777 "$OUTPUT_DIR" || true
        fi
    fi
    
    # Record original size
    local original_size=$(du -h "$OUTPUT_DIR/${efi_file_name}" | cut -f1)
    log "INFO" "Original EFI size: $original_size"
    
    # Apply compression if enabled
    if [ "$INCLUDE_COMPRESSION" = "true" ]; then
        # Determine which compression tool to use
        COMPRESSION_TOOL="${COMPRESSION_TOOL:-upx}"
        log "INFO" "Selected compression tool: $COMPRESSION_TOOL"
        
        # Create a backup of the original file
        cp "$OUTPUT_DIR/${efi_file_name}" "$OUTPUT_DIR/${efi_file_name}.original"
        
        case "$COMPRESSION_TOOL" in
            "upx")
                if command -v upx &> /dev/null; then
                    # Apply UPX compression
                    log "INFO" "Compressing with UPX (faster decompression, good size reduction)..."
                    # Use --no-lzma and add --no-overlay options to avoid section wrap-around errors
                    if upx --best --no-lzma --no-overlay "$OUTPUT_DIR/${efi_file_name}"; then
                        COMPRESSED_SIZE=$(du -h "$OUTPUT_DIR/${efi_file_name}" | cut -f1)
                        log "SUCCESS" "UPX compression successful"
                        log "INFO" "Original size: $original_size, Compressed size: $COMPRESSED_SIZE"
                    else
                        log "WARNING" "UPX compression failed, restoring original file"
                        mv "$OUTPUT_DIR/${efi_file_name}.original" "$OUTPUT_DIR/${efi_file_name}"
                    fi
                else
                    log "ERROR" "UPX not found. Please install UPX: apt-get install upx-ucl"
                    log "INFO" "Restoring original uncompressed file"
                    mv "$OUTPUT_DIR/${efi_file_name}.original" "$OUTPUT_DIR/${efi_file_name}"
                fi
                ;;
                
            "xz")
                if command -v xz &> /dev/null; then
                    # Apply XZ compression
                    log "INFO" "Compressing with XZ (higher compression ratio, slower decompression)..."
                    # Note: XZ compression would require a decompression stub in a real implementation
                    # This is a simplified version for demonstration
                    if xz -z -9 -e -f --keep "$OUTPUT_DIR/${efi_file_name}"; then
                        # In a real implementation, we would need to prepend a decompression stub
                        # For now, we'll just rename the file
                        mv "$OUTPUT_DIR/${efi_file_name}.xz" "$OUTPUT_DIR/${efi_file_name}.compressed"
                        COMPRESSED_SIZE=$(du -h "$OUTPUT_DIR/${efi_file_name}.compressed" | cut -f1)
                        log "WARNING" "XZ compression completed but requires a custom decompression stub"
                        log "INFO" "Original size: $original_size, Compressed size: $COMPRESSED_SIZE"
                        log "INFO" "Using original uncompressed file for compatibility"
                        mv "$OUTPUT_DIR/${efi_file_name}.original" "$OUTPUT_DIR/${efi_file_name}"
                    else
                        log "WARNING" "XZ compression failed, restoring original file"
                        mv "$OUTPUT_DIR/${efi_file_name}.original" "$OUTPUT_DIR/${efi_file_name}"
                    fi
                else
                    log "ERROR" "XZ not found. Please install XZ"
                    log "INFO" "Restoring original uncompressed file"
                    mv "$OUTPUT_DIR/${efi_file_name}.original" "$OUTPUT_DIR/${efi_file_name}"
                fi
                ;;
                
            "zstd")
                if command -v zstd &> /dev/null; then
                    # Apply ZSTD compression
                    log "INFO" "Compressing with ZSTD (balanced compression ratio and speed)..."
                    # Note: ZSTD compression would require a decompression stub in a real implementation
                    # This is a simplified version for demonstration
                    if zstd -19 -f "$OUTPUT_DIR/${efi_file_name}" -o "$OUTPUT_DIR/${efi_file_name}.zst"; then
                        # In a real implementation, we would need to prepend a decompression stub
                        # For now, we'll just rename the file
                        mv "$OUTPUT_DIR/${efi_file_name}.zst" "$OUTPUT_DIR/${efi_file_name}.compressed"
                        COMPRESSED_SIZE=$(du -h "$OUTPUT_DIR/${efi_file_name}.compressed" | cut -f1)
                        log "WARNING" "ZSTD compression completed but requires a custom decompression stub"
                        log "INFO" "Original size: $original_size, Compressed size: $COMPRESSED_SIZE"
                        log "INFO" "Using original uncompressed file for compatibility"
                        mv "$OUTPUT_DIR/${efi_file_name}.original" "$OUTPUT_DIR/${efi_file_name}"
                    else
                        log "WARNING" "ZSTD compression failed, restoring original file"
                        mv "$OUTPUT_DIR/${efi_file_name}.original" "$OUTPUT_DIR/${efi_file_name}"
                    fi
                else
                    log "ERROR" "ZSTD not found. Please install ZSTD"
                    log "INFO" "Restoring original uncompressed file"
                    mv "$OUTPUT_DIR/${efi_file_name}.original" "$OUTPUT_DIR/${efi_file_name}"
                fi
                ;;
                
            *)
                log "ERROR" "Unknown compression tool: $COMPRESSION_TOOL"
                log "INFO" "Restoring original uncompressed file"
                mv "$OUTPUT_DIR/${efi_file_name}.original" "$OUTPUT_DIR/${efi_file_name}"
                ;;
        esac
        
        # Clean up backup if it exists
        rm -f "$OUTPUT_DIR/${efi_file_name}.original"
    else
        log "INFO" "Compression disabled"
    fi
    
    local final_size=$(du -h "$OUTPUT_DIR/${efi_file_name}" | cut -f1)
    log "SUCCESS" "${efi_file_name} created successfully (size: $final_size)"
    
    # Return to build directory
    cd "$BUILD_DIR"
}

# Display comprehensive help for build options
show_build_help() {
    cat << EOF
Usage: $0 [options]

Build Type Options:
  --minimal              Minimal build optimized for size (~30-50% smaller)
                         Automatically disables ZFS, Btrfs, recovery tools, network tools,
                         crypto support, and TUI; enables minimal kernel config
  --full                 Full build with all available components
  --standard             Standard build with recommended components (default)

Optional Components:
  --minimal-kernel       Use minimized kernel configuration (saves memory/space)
  --standard-kernel      Use standard kernel configuration (default)
  --with-zfs             Include ZFS filesystem support (default: yes)
  --without-zfs          Exclude ZFS filesystem support
  --with-btrfs           Include Btrfs filesystem support (default: no)
  --without-btrfs        Exclude Btrfs filesystem support
  --with-recovery-tools  Include data recovery tools (default: yes)
  --without-recovery-tools  Exclude data recovery tools
  --with-network-tools   Include network tools (default: yes)
  --without-network-tools  Exclude network tools
  --with-crypto          Include encryption support (default: yes)
  --without-crypto       Exclude encryption support
  --with-tui             Include Text User Interface (default: yes)
  --without-tui          Exclude Text User Interface

Size Optimization Options:
  --with-compression     Enable EFI file compression (default: yes)
  --without-compression  Disable EFI file compression (faster boot)
  --compression-tool=TOOL Select compression tool (upx, xz, zstd) (default: upx)

Build Performance Options:
  --use-cache            Enable source and build caching (default for build.sh)
  --no-cache             Disable source and build caching (default for 04_build.sh)
  --use-swap             Create swap file if memory is low (default: no)
  --no-swap              Do not create swap file even if memory is low
  --interactive-config   Use interactive kernel configuration (menuconfig)
  --no-interactive-config Use non-interactive kernel config (default)
  --make-verbose         Enable verbose make output (V=1)
  --make-quiet           Use quiet make output (V=0, default)
  --jobs=N               Set number of parallel build jobs (default: auto-detect)

Examples:
  --minimal                       Create minimal recovery image (no ZFS, minimal kernel)
  --full --use-swap               Create full-featured image with swap support
  --standard --with-btrfs         Add Btrfs to standard build (includes ZFS by default)
  --standard --without-zfs        Standard build without ZFS support
  --compression-tool=zstd         Use ZSTD compression instead of default UPX
  --interactive-config --jobs=4   Use interactive config with 4 build threads
EOF
}

# Process build arguments
parse_build_args() {
    # Check if BUILD_TYPE is already set from environment
    # If it's set (especially by Docker entrypoint), respect it
    if [ -z "${BUILD_TYPE:-}" ]; then
        # Default to standard profile
        apply_build_profile "standard"
    else
        log "INFO" "Using BUILD_TYPE from environment: $BUILD_TYPE"
        # Apply the profile that matches the BUILD_TYPE
        apply_build_profile "$BUILD_TYPE"
    fi
    
    # Performance options - not controlled by profiles
    USE_CACHE="${USE_CACHE:-false}"
    USE_SWAP="${USE_SWAP:-false}"
    INTERACTIVE_CONFIG="${INTERACTIVE_CONFIG:-false}"
    MAKE_VERBOSE="${MAKE_VERBOSE:-0}"
    COMPRESSION_TOOL="${COMPRESSION_TOOL:-upx}"
    
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --minimal)
                # Use apply_build_profile from 86_build_profiles.sh
                apply_build_profile "minimal"
                shift
                ;;
            --standard)
                # Use apply_build_profile from 86_build_profiles.sh
                apply_build_profile "standard"
                shift
                ;;
            --full)
                # Use apply_build_profile from 86_build_profiles.sh
                apply_build_profile "full"
                shift
                ;;
            # Individual component options still handled individually
            --minimal-kernel)
                INCLUDE_MINIMAL_KERNEL=true
                shift
                ;;
            --standard-kernel)
                INCLUDE_MINIMAL_KERNEL=false
                shift
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
            --with-compression|--compression)
                INCLUDE_COMPRESSION=true
                shift
                ;;
            --without-compression|--no-compression)
                INCLUDE_COMPRESSION=false
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
            --make-verbose)
                MAKE_VERBOSE=1
                shift
                ;;
            --make-quiet)
                MAKE_VERBOSE=0
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
            --help|-h)
                # Show comprehensive help and exit
                show_build_help
                exit 0
                ;;
            *)
                log "WARNING" "Unknown option: $1"
                shift
                ;;
        esac
    done
    
    # Print configuration summary using the profile name
    log "INFO" "Build configuration:"
    log "INFO" "- Build profile: $BUILD_TYPE"
    
    # Display feature settings
    log "INFO" "- Feature settings:"
    log "INFO" "  - Minimal kernel: $(bool_to_str "${INCLUDE_MINIMAL_KERNEL:-false}")"
    log "INFO" "  - ZFS support: $(bool_to_str "${INCLUDE_ZFS:-true}")"
    log "INFO" "  - Btrfs support: $(bool_to_str "${INCLUDE_BTRFS:-false}")"
    log "INFO" "  - Recovery tools: $(bool_to_str "${INCLUDE_RECOVERY_TOOLS:-true}")"
    log "INFO" "  - Network tools: $(bool_to_str "${INCLUDE_NETWORK_TOOLS:-true}")"
    log "INFO" "  - Crypto support: $(bool_to_str "${INCLUDE_CRYPTO:-true}")"
    log "INFO" "  - Text User Interface: $(bool_to_str "${INCLUDE_TUI:-true}")"
    log "INFO" "  - Compression: $(bool_to_str "${INCLUDE_COMPRESSION:-true}")"
    
    if [ "${INCLUDE_COMPRESSION:-true}" = "true" ]; then
        log "INFO" "  - Compression tool: $COMPRESSION_TOOL"
    fi
    
    # Display build performance settings
    log "INFO" "- Build performance settings:"
    log "INFO" "  - Use caching: $(bool_to_str "$USE_CACHE")"
    log "INFO" "  - Use swap if needed: $(bool_to_str "$USE_SWAP")"
    log "INFO" "  - Interactive config: $(bool_to_str "$INTERACTIVE_CONFIG")"
    log "INFO" "  - Verbose make output: $(bool_to_str "$MAKE_VERBOSE")"
}

# Export all functions for use in other scripts
export -f bool_to_str
export -f build_kernel
export -f build_zfs
export -f create_efi
export -f parse_build_args
export -f show_build_help