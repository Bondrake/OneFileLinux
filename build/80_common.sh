#!/bin/bash
#
# OneFileLinux Common Functions (80_common.sh)
# Shared basic utilities used across all build scripts
# This is part of the library scripts (80-89 range)
#

# Color codes for better visibility
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Version information
ALPINE_VERSION="3.21"      # Alpine Linux version to use
KERNEL_VERSION="6.12.19"   # Linux kernel version

# Log file for build errors
BUILD_LOG="build_error.log"

# Function to print messages with optional timestamp and colors
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

# Print OneFileLinux banner
print_banner() {
    local script_name=${1:-$SCRIPT_NAME}
    echo -e "${BLUE}"
    echo "      ____________  "
    echo "    /|------------| "
    echo "   /_|  .---.     | "
    echo "  |    /     \    | "
    echo "  |    \.6-6./    | "
    echo "  |    /\`\_/\`\    | "
    echo "  |   //  _  \\\   | "
    echo "  |  | \     / |  | "
    echo "  | /\`\_\`>  <_/\`\ | "
    echo "  | \__/'---'\__/ | "
    echo "  |_______________| "
    echo "                    "
    echo -e "${GREEN}   OneFileLinux: $script_name  ${NC}"
    echo "----------------------------------------------------"
}

# Function to print script end
print_script_end() {
    echo "----------------------------------------------------"
    log "SUCCESS" "$SCRIPT_NAME completed successfully"
    echo "----------------------------------------------------"
}

# Function to print section header
print_section() {
    local title=$1
    echo "===================================================================="
    echo "  $title"
    echo "===================================================================="
}

# Environment detection functions
# =========================

# Check if we're running in a container environment
is_container() {
    # First check the environment variable set by the entrypoint script
    if [ "${IN_DOCKER_CONTAINER}" = "true" ]; then
        return 0
    fi
    
    # Check if we're in GitHub Actions
    if [ -n "${GITHUB_ACTIONS:-}" ]; then
        return 0
    fi
    
    # Fallback to traditional detection methods
    grep -q "docker\|container" /proc/1/cgroup 2>/dev/null || [ -f "/.dockerenv" ]
    return $?
}

# Check if we're running in GitHub Actions specifically
is_github_actions() {
    [ -n "${GITHUB_ACTIONS:-}" ]
}

# Check if we're running in a Docker container
is_docker_container() {
    [ "${IN_DOCKER_CONTAINER}" = "true" ] || grep -q "docker\|container" /proc/1/cgroup 2>/dev/null || [ -f "/.dockerenv" ]
}

# Check if we're in any restricted environment 
is_restricted_environment() {
    is_github_actions || is_container
}

# Create a global associative array to track loaded libraries
declare -A ONEFILELINUX_LIBRARIES_LOADED 2>/dev/null || {
    # Fallback for older bash versions that don't support associative arrays
    ONEFILELINUX_LIBRARIES_LOADED=()
}

# Function to check if a library has been loaded
is_library_loaded() {
    local library_name="$1"
    
    # Extract just the filename without path and extension
    local basename=$(basename "$library_name" .sh)
    
    # Check if it's in our tracking array
    if [ "${ONEFILELINUX_LIBRARIES_LOADED[$basename]:-}" = "1" ]; then
        return 0  # Already loaded
    fi
    return 1  # Not loaded
}

# Function to mark a library as loaded
mark_library_loaded() {
    local library_name="$1"
    
    # Extract just the filename without path and extension
    local basename=$(basename "$library_name" .sh)
    
    # Mark it as loaded in our tracking array 
    ONEFILELINUX_LIBRARIES_LOADED[$basename]="1"
    
    # For backwards compatibility, maintain the string-based tracking
    if [ -z "${LIBRARIES_LOADED:-}" ]; then
        export LIBRARIES_LOADED="$basename"
    else
        export LIBRARIES_LOADED="$LIBRARIES_LOADED:$basename"
    fi
}

# Function to safely source library files with error checking
source_library() {
    local library_file=$1
    local required=${2:-false}
    
    # Extract just the filename without path and extension
    local basename=$(basename "$library_file" .sh)
    
    # Skip if already loaded
    if is_library_loaded "$basename"; then
        local calling_script=$(caller | awk '{print $2}')
        local calling_line=$(caller | awk '{print $1}')
        echo -e "${BLUE}[INFO]${NC} Library $basename already loaded (referenced from $calling_script:$calling_line)"
        return 0
    fi
    
    if [ -f "$library_file" ]; then
        # Source the library
        source "$library_file"
        # Mark it as loaded
        mark_library_loaded "$basename"
        return 0
    else
        if [ "$required" = "true" ]; then
            echo -e "${RED}[ERROR]${NC} Required library file not found: $library_file"
            exit 1
        else
            echo -e "${YELLOW}[WARNING]${NC} Library file not found: $library_file"
            return 1
        fi
    fi
}

# Function to initialize all standard libraries
source_libraries() {
    local script_path=${1:-"."}
    
    # Mark 80_common as loaded if not already (self-reference)
    if ! is_library_loaded "80_common"; then
        mark_library_loaded "80_common"
    fi
    
    # Load standard libraries with tracking
    
    # Error handling (required)
    if source_library "$script_path/81_error_handling.sh"; then
        # Initialize error handling if available - the function itself handles deduplication
        init_error_handling
    else
        echo -e "${YELLOW}[WARNING]${NC} Error handling is limited"
    fi
    
    # Build helpers
    source_library "$script_path/82_build_helper.sh"
    
    # After loading build helper, process any existing build flags from environment
    if declare -f parse_build_flags >/dev/null; then
        # Get existing build args from environment
        if [ -n "${BUILD_ARGS:-}" ]; then
            echo -e "${BLUE}[INFO]${NC} Processing BUILD_ARGS from environment: $BUILD_ARGS"
            parse_build_flags "$BUILD_ARGS" true
        fi
    fi
    
    # Optional configuration helpers
    source_library "$script_path/83_config_helper.sh" false
    
    # Build core library (if available)
    if [ -f "$script_path/84_build_core.sh" ]; then
        # Ensure it's executable
        if [ ! -x "$script_path/84_build_core.sh" ]; then
            chmod +x "$script_path/84_build_core.sh" 2>/dev/null || 
                echo -e "${YELLOW}[WARNING]${NC} Could not make build core library executable"
        fi
        
        source_library "$script_path/84_build_core.sh"
    fi
    
    # Dependency helper library (if available)
    if [ -f "$script_path/85_dependency_helper.sh" ]; then
        # Ensure it's executable
        if [ ! -x "$script_path/85_dependency_helper.sh" ]; then
            chmod +x "$script_path/85_dependency_helper.sh" 2>/dev/null || 
                echo -e "${YELLOW}[WARNING]${NC} Could not make dependency helper library executable"
        fi
        
        source_library "$script_path/85_dependency_helper.sh"
    fi
    
    # Debug information about loaded libraries
    if [ "${DEBUG_LIBRARY_LOADING:-}" = "true" ]; then
        local calling_script=$(caller | awk '{print $2}')
        local calling_line=$(caller | awk '{print $1}')
        local loaded_libs=""
        for lib in "${!ONEFILELINUX_LIBRARIES_LOADED[@]}"; do
            loaded_libs="$loaded_libs $lib"
        done
        echo -e "${BLUE}[DEBUG]${NC} Current loaded libraries:$loaded_libs (from $calling_script:$calling_line)"
    fi
    
    # Debug information about feature flags
    if [ "${DEBUG_LIBRARY_LOADING:-}" = "true" ] || [ "${DEBUG_FEATURE_FLAGS:-}" = "true" ]; then
        echo -e "${BLUE}[DEBUG]${NC} Feature flags: INCLUDE_MINIMAL_KERNEL=${INCLUDE_MINIMAL_KERNEL:-false}, INCLUDE_ZFS=${INCLUDE_ZFS:-true}, INCLUDE_NETWORK_TOOLS=${INCLUDE_NETWORK_TOOLS:-true}, INCLUDE_CRYPTO=${INCLUDE_CRYPTO:-true}"
    fi
    
    return 0
}

# Function to initialize the script with a standard header
# This should be the ONLY place that prints the banner
initialize_script() {
    # Print the banner
    print_banner
    
    # Set flag to prevent duplicate banners
    export BANNER_PRINTED=true
}

# Timing functions for build performance tracking
# ==========================================

# Global timing variables
GLOBAL_BUILD_START_TIME=$(date +%s)

# Get script directory for absolute paths
COMMON_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Define timing log with absolute path for consistency across scripts
TIMING_LOG_FILE="${COMMON_DIR}/build_timing.log"

# Timing function for build steps
log_timing() {
    local step_name="$1"
    local step_duration="$2"
    local log_file="${TIMING_LOG_FILE}"
    
    # Create log file with header if it doesn't exist
    if [ ! -f "$log_file" ]; then
        echo "==========================================" > "$log_file"
        echo "OneFileLinux Build Timing Log" >> "$log_file"
        echo "Started at: $(date)" >> "$log_file"
        echo "==========================================" >> "$log_file"
        echo "" >> "$log_file"
        echo "STEP                               DURATION" >> "$log_file"
        echo "----------------------------------------" >> "$log_file"
    fi
    
    # Format timing information (right-aligned)
    local formatted_time=$(printf "%-35s %6ds" "$step_name" "$step_duration")
    echo "$formatted_time" >> "$log_file"
    
    # Also log to console
    log "INFO" "[TIMING] $step_name: ${step_duration}s"
}

# Start timing for a step
start_timing() {
    STEP_NAME="$1"
    STEP_START_TIME=$(date +%s)
    log "INFO" "[TIMING] Starting: $STEP_NAME"
}

# End timing for a step
end_timing() {
    local current_time=$(date +%s)
    local duration=$((current_time - STEP_START_TIME))
    log_timing "$STEP_NAME" "$duration"
}

# Finalize timing log with summary
finalize_timing_log() {
    local build_end_time=$(date +%s)
    local build_duration=$((build_end_time - GLOBAL_BUILD_START_TIME))
    local build_minutes=$((build_duration / 60))
    local build_seconds=$((build_duration % 60))
    
    # Add total build time to timing log
    log_timing "TOTAL BUILD TIME" "$build_duration"
    
    # Add footer to timing log
    echo "" >> "${TIMING_LOG_FILE}"
    echo "==========================================" >> "${TIMING_LOG_FILE}"
    echo "Build completed at: $(date)" >> "${TIMING_LOG_FILE}"
    echo "Total build time: ${build_minutes}m ${build_seconds}s" >> "${TIMING_LOG_FILE}"
    echo "==========================================" >> "${TIMING_LOG_FILE}"
    
    log "INFO" "Detailed timing log saved to: ${TIMING_LOG_FILE}"
    
    # Don't return the build duration as an exit code - this is causing the build to exit with build time as code
    return 0
}

# Export all functions for use in other scripts
export -f log
export -f print_banner
export -f print_section
export -f is_container
export -f is_github_actions
export -f is_docker_container
export -f is_restricted_environment
export -f source_library
export -f source_libraries
export -f initialize_script
export -f log_timing
export -f start_timing
export -f end_timing
export -f finalize_timing_log