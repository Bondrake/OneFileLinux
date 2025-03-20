#!/bin/bash
#
# Build Linux kernel and create EFI file
#
# Core build process that compiles the kernel and produces the EFI file
#
set -e

# Define script name for error handling
SCRIPT_NAME=$(basename "$0")

# Determine the absolute path to the script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Source the core library first (required)
if [ ! -f "${SCRIPT_DIR}/80_common.sh" ]; then
    echo "ERROR: Critical library file not found: ${SCRIPT_DIR}/80_common.sh"
    exit 1
fi
source "${SCRIPT_DIR}/80_common.sh"

# Source all library scripts using the source_libraries function
source_libraries "${SCRIPT_DIR}"

# Initialize script with standard header (prints banner)
initialize_script

# Define paths
BUILD_DIR="$SCRIPT_DIR"
ROOTFS_DIR="$BUILD_DIR/alpine-minirootfs"
KERNEL_DIR="$BUILD_DIR/linux"
ZFILES_DIR="$BUILD_DIR/zfiles"
OUTPUT_DIR="$BUILD_DIR/../output"

# Export paths for use by the build_core library functions
export BUILD_DIR ROOTFS_DIR KERNEL_DIR ZFILES_DIR OUTPUT_DIR

# Ensure output directory exists
mkdir -p "$OUTPUT_DIR"

# Add debugging for Docker builds
echo "DEBUG: 04_build.sh paths:"
echo "  BUILD_DIR=$BUILD_DIR"
echo "  OUTPUT_DIR=$OUTPUT_DIR"

# Main function
main() {
    # Set up the timing log file with absolute path
    TIMING_LOG_FILE="${BUILD_DIR}/build_timing.log"
    export TIMING_LOG_FILE
    
    # Reset timing log for a clean run
    rm -f "$TIMING_LOG_FILE"
    
    # Print environment information if the function exists
    if type print_environment_info &>/dev/null; then
        print_environment_info
    fi
    
    # Parse command line arguments
    start_timing "Argument parsing"
    parse_build_args "$@"
    end_timing
    
    # Make sure we're in the build directory
    cd "$BUILD_DIR"
    
    # Verify prerequisites - no fallbacks, just fail with clear error messages
    start_timing "Prerequisite verification"
    if [ ! -d "$ROOTFS_DIR" ]; then
        log "ERROR" "Alpine rootfs directory not found: $ROOTFS_DIR"
        log "INFO" "Please run the complete build sequence:"
        log "INFO" "cd build && ./01_get.sh && ./02_chrootandinstall.sh && ./03_conf.sh"
        exit 1
    fi
    
    # Verify the kernel sources exist
    if [ ! -d "$KERNEL_DIR" ]; then
        log "ERROR" "Kernel directory not found: $KERNEL_DIR"
        log "INFO" "Please run 01_get.sh first"
        exit 1
    fi
    
    # ZFS now uses pre-built Alpine packages, no need to check for source directory
    if [ "$INCLUDE_ZFS" = "true" ]; then
        log "INFO" "ZFS support will use pre-built Alpine packages"
    fi
    
    # Make sure kernel is configured
    if [ ! -f "$KERNEL_DIR/.config" ]; then
        log "ERROR" "Kernel configuration not found: $KERNEL_DIR/.config"
        log "INFO" "Please run 03_conf.sh first"
        exit 1
    fi
    end_timing
    
    # ABI header warnings suppressed
    log "INFO" "ABI header mismatch warnings are allowed for now"

    # Build kernel
    start_timing "Kernel build"
    build_kernel || {
        log "ERROR" "Kernel build failed"
        exit 1
    }
    end_timing
    
    # Build ZFS if enabled
    if [ "$INCLUDE_ZFS" = "true" ]; then
        start_timing "ZFS build"
        build_zfs || {
            log "ERROR" "ZFS build failed"
            exit 1
        }
        end_timing
    fi
    
    # Create EFI file
    start_timing "EFI file creation"
    create_efi || {
        log "ERROR" "Failed to create EFI file"
        exit 1
    }
    end_timing
    
    # Finalize timing log with summary (only if this is the final script)
    if [ "${FINALIZE_TIMING_LOG:-false}" = "true" ]; then
        finalize_timing_log
    fi
    
    # Print build summary
    print_section "Build Summary"
    log "SUCCESS" "Build completed successfully!"
    log "INFO" "Detailed timing log saved to: ${TIMING_LOG_FILE}"
    
    # First check for profile-specific EFI file
    local efi_filename=""
    
    # Check if we have an expected EFI filename from create_efi function
    if [ -n "${EXPECTED_EFI_FILENAME:-}" ]; then
        efi_filename="$EXPECTED_EFI_FILENAME"
    # Use BUILD_TYPE if available
    elif [ -n "${BUILD_TYPE:-}" ]; then
        efi_filename="OneFileLinux-${BUILD_TYPE}.efi"
    # Fall back to generic name if needed
    else
        efi_filename="OneFileLinux.efi"
    fi
    
    if [ -f "$OUTPUT_DIR/$efi_filename" ]; then
        local file_size=$(du -h "$OUTPUT_DIR/$efi_filename" | cut -f1)
        log "SUCCESS" "Created $efi_filename (Size: $file_size)"
        log "INFO" "EFI file: $OUTPUT_DIR/$efi_filename"
        
        # Show included features
        log "INFO" "Included features:"
        [ "$INCLUDE_ZFS" = "true" ] && log "INFO" "  - ZFS filesystem support"
        [ "$INCLUDE_BTRFS" = "true" ] && log "INFO" "  - Btrfs filesystem support"
        [ "$INCLUDE_RECOVERY_TOOLS" = "true" ] && log "INFO" "  - Data recovery tools"
        [ "$INCLUDE_NETWORK_TOOLS" = "true" ] && log "INFO" "  - Network tools"
        [ "$INCLUDE_CRYPTO" = "true" ] && log "INFO" "  - Encryption support"
        [ "$INCLUDE_TUI" = "true" ] && log "INFO" "  - Text User Interface"
        
        # Show build configuration
        log "INFO" ""
        log "INFO" "Build configuration:"
        if [ "$INCLUDE_MINIMAL_KERNEL" = "true" ]; then
            log "INFO" "  - Kernel: Minimal (optimized for size)"
        else
            log "INFO" "  - Kernel: Standard"
        fi
        
        if [ "$INCLUDE_COMPRESSION" = "true" ]; then
            log "INFO" "  - Compression: Enabled (using ${COMPRESSION_TOOL:-upx})"
        else
            log "INFO" "  - Compression: Disabled"
        fi
        
        # Show cache information if used
        if [ "${USE_CACHE:-false}" = "true" ] && command -v ccache &> /dev/null; then
            log "INFO" ""
            log "INFO" "Compiler cache statistics:"
            ccache -s | grep -E 'cache hit|cache miss|cache size' | while read line; do
                log "INFO" "  $line"
            done
        fi
    else
        log "ERROR" "Output file not found. Build may have failed silently."
        exit 1
    fi
}

# Execute main function with all arguments
main "$@"