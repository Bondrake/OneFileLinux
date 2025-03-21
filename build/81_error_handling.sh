#!/bin/bash
#
# OneFileLinux Error Handling Framework (81_error_handling.sh)
# Provides error handling, recovery options, and prerequisite checks
# This is part of the library scripts (80-89 range)
#

# Color codes for better visibility
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Log file for build errors
BUILD_LOG="build_error.log"

# Function to print with timestamp
log() {
    local level=$1
    local message=$2
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    
    case "$level" in
        "INFO")
            echo -e "${BLUE}[INFO]${NC} $message"
            ;;
        "WARNING")
            echo -e "${YELLOW}[WARNING]${NC} $message"
            ;;
        "ERROR")
            echo -e "${RED}[ERROR]${NC} $message"
            # Write to log file with error handling
            if ! echo "[$timestamp] [ERROR] $message" >> "$BUILD_LOG" 2>/dev/null; then
                echo -e "${YELLOW}[WARNING]${NC} Could not write to log file. Continuing without logging."
            fi
            ;;
        "SUCCESS")
            echo -e "${GREEN}[SUCCESS]${NC} $message"
            ;;
        *)
            echo -e "$message"
            ;;
    esac
}

# Function to handle errors in standard environments
handle_error() {
    local err_code=$?
    local line_num=$1
    local command="$2"
    
    # Log exit code information for debugging
    echo "DEBUG: Error handler activated - exit code: $err_code, command: '$command', line: $line_num, script: $SCRIPT_NAME"
    
    if [ $err_code -ne 0 ]; then
        log "ERROR" "Command failed with exit code $err_code at line $line_num: $command"
        log "ERROR" "Check $BUILD_LOG for details"
        
        # Attempt to provide helpful debug information
        case "$command" in
            *wget*)
                log "ERROR" "Network error or invalid URL. Check your internet connection."
                ;;
            *tar*)
                log "ERROR" "Archive extraction failed. The downloaded file may be corrupted."
                log "ERROR" "Try removing the file and running the script again."
                ;;
            *chroot*)
                log "ERROR" "Chroot failed. This may be due to permission issues or missing binaries."
                log "ERROR" "Make sure you are running as root/sudo."
                ;;
            *make*)
                log "ERROR" "Build failed. Check if all dependencies are installed."
                log "ERROR" "Run 00_prepare.sh again to ensure all required packages are installed."
                ;;
            *)
                log "ERROR" "Command failed. See error message above for details."
                ;;
        esac
        
        # Offer recovery options
        echo ""
        log "INFO" "To recover:"
        case "$SCRIPT_NAME" in
            "01_get.sh")
                log "INFO" "- Ensure your internet connection is working"
                log "INFO" "- Remove any partial downloaded files"
                log "INFO" "- Run ./01_get.sh again"
                ;;
            "02_chrootandinstall.sh")
                log "INFO" "- Ensure you have root/sudo privileges"
                log "INFO" "- Make sure alpine-minirootfs was extracted correctly"
                log "INFO" "- Run ./02_chrootandinstall.sh again"
                ;;
            "03_conf.sh")
                log "INFO" "- Check if the files in zfiles/ directory exist"
                log "INFO" "- Make sure alpine-minirootfs was configured correctly"
                log "INFO" "- Run ./03_conf.sh again"
                ;;
            "04_build.sh")
                log "INFO" "- Make sure all build dependencies are installed with ./00_prepare.sh"
                log "INFO" "- Check if kernel source was extracted correctly"
                log "INFO" "- For kernel build errors, see output above for specific errors"
                log "INFO" "- You can try running './04_build.sh' again"
                ;;
            *)
                log "INFO" "- Check the error message above"
                log "INFO" "- Fix the issue and try again"
                ;;
        esac
        
        exit $err_code
    fi
}

# Set up error trapping
trap_errors() {
    # In all environments, we use the same error handler with built-in container detection
    # Note: We've removed the log message here since it's handled by init_error_handling
    
    # Still use set -e, but our error handler will decide whether to continue
    # based on the error type and environment
    set -e
    trap 'handle_error ${LINENO} "${BASH_COMMAND}"' ERR
}

# Check if prerequisites are met for this specific script
check_prerequisites() {
    case "$SCRIPT_NAME" in
        "01_get.sh")
            # Check for wget
            if ! command -v wget &> /dev/null; then
                log "ERROR" "wget not found. Install wget and try again."
                log "INFO" "Run ./00_prepare.sh to install required dependencies."
                exit 1
            fi
            ;;
        "02_chrootandinstall.sh")
            # Check for chroot capability
            if [ "$EUID" -ne 0 ]; then
                log "ERROR" "This script requires root/sudo privileges for chroot."
                log "INFO" "Run with sudo: sudo ./02_chrootandinstall.sh"
                exit 1
            fi
            
            # Check if minirootfs exists
            if [ ! -d "alpine-minirootfs" ]; then
                log "ERROR" "alpine-minirootfs directory not found."
                log "INFO" "Run ./01_get.sh first to download Alpine Linux."
                exit 1
            fi
            ;;
        "03_conf.sh")
            # Check if minirootfs exists
            if [ ! -d "alpine-minirootfs" ]; then
                log "ERROR" "alpine-minirootfs directory not found."
                log "INFO" "Run ./01_get.sh and ./02_chrootandinstall.sh first."
                exit 1
            fi
            
            # Check if zfiles exists
            if [ ! -d "zfiles" ]; then
                log "ERROR" "zfiles directory not found."
                exit 1
            fi
            ;;
        "04_build.sh")
            # Check if kernel source exists
            if [ ! -d "linux" ]; then
                log "ERROR" "linux directory not found."
                log "INFO" "Run ./01_get.sh first to download Linux kernel."
                exit 1
            fi
            
            # Check for build tools
            if ! command -v make &> /dev/null; then
                log "ERROR" "make not found. Install build tools and try again."
                log "INFO" "Run ./00_prepare.sh to install required dependencies."
                exit 1
            fi
            ;;
    esac
}

# Use a global variable to ensure we only initialize once per session
ONEFILELINUX_ERROR_HANDLING_INITIALIZED=${ONEFILELINUX_ERROR_HANDLING_INITIALIZED:-false}

# Initialize error handling framework
init_error_handling() {
    # Skip initialization if already done - use a simple global flag
    if [ "$ONEFILELINUX_ERROR_HANDLING_INITIALIZED" = "true" ]; then
        # Add debug message if we're in DEBUG mode
        if [ "${DEBUG_LIBRARY_LOADING:-false}" = "true" ]; then
            echo -e "${BLUE}[DEBUG]${NC} Error handling already initialized, skipping"
        fi
        return 0
    fi
    
    # Try to create the log file with proper permissions
    if ! touch "$BUILD_LOG" 2>/dev/null; then
        echo -e "${YELLOW}[WARNING]${NC} Cannot create log file. Will continue without logging."
        BUILD_LOG="/dev/null"
    else
        # Make sure the log file is writable
        chmod 666 "$BUILD_LOG" 2>/dev/null || true
        echo "" > "$BUILD_LOG" 2>/dev/null || true
    fi
    
    # Export critical functions for use in other scripts
    # Export these immediately upon definition for Docker environments
    type print_script_end >/dev/null 2>&1 && export -f print_script_end || true
    type print_script_start >/dev/null 2>&1 && export -f print_script_start || true
    
    trap_errors
    check_prerequisites
    
    # No caller information needed in normal operation
    
    # Simple message about error handling setup
    echo -e "${BLUE}[INFO]${NC} Setting up error handling"
    
    # Mark as initialized - this is the official flag we'll check from now on
    ONEFILELINUX_ERROR_HANDLING_INITIALIZED=true
    export ONEFILELINUX_ERROR_HANDLING_INITIALIZED
    
    # We'll let the main script handle printing the banner
    # This avoids duplicate banners when scripts call initialize_script
}

# Export key functions for use in other scripts
export -f init_error_handling
export -f handle_error
export -f trap_errors
export -f check_prerequisites