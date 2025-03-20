#!/bin/bash
#
# Download Alpine Linux, Linux kernel, and ZFS sources
#

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

# Define component versions using centralized variables from 80_common.sh
alpineminirootfsfile="alpine-minirootfs-3.21.3-x86_64.tar.gz"  # Specific point release for the rootfs
linuxver="linux-${KERNEL_VERSION}"
zfsver="${ZFS_VERSION}"

# Define download URLs
ALPINE_URL="http://dl-cdn.alpinelinux.org/alpine/v${ALPINE_VERSION}/releases/x86_64/$alpineminirootfsfile"
KERNEL_URL="http://cdn.kernel.org/pub/linux/kernel/v6.x/$linuxver.tar.xz"
ZFS_URL="https://github.com/openzfs/zfs/releases/download/zfs-${zfsver}/zfs-${zfsver}.tar.gz"

# Function to download and verify a file
download_and_verify() {
    local url=$1
    local file=$(basename "$url")
    local component=$2
    
    # Use cache if enabled
    if [ "${USE_CACHE:-false}" = "true" ]; then
        local cache_file="$CACHE_DIR/sources/$file"
        
        if [ -f "$cache_file" ]; then
            log "INFO" "Using cached $component from $cache_file"
            
            # Copy from cache to working directory
            cp "$cache_file" ./ || {
                log "WARNING" "Failed to copy from cache, will download instead"
                download_file "$url" "$file" "$component"
            }
        else
            # Download and store in cache
            log "INFO" "Downloading $component from $url (will cache)"
            
            wget -c4 "$url" || {
                log "ERROR" "Failed to download $component from $url"
                return 1
            }
            
            # Store in cache
            mkdir -p "$CACHE_DIR/sources"
            cp "$file" "$cache_file" || log "WARNING" "Failed to cache $file"
        fi
    else
        # Regular download without caching
        download_file "$url" "$file" "$component"
    fi
    
    log "SUCCESS" "Downloaded $component successfully"
    return 0
}

# Function to download file without caching
download_file() {
    local url=$1
    local file=$2
    local component=$3
    
    log "INFO" "Downloading $component from $url"
    
    # Skip download if file already exists
    if [ -f "$file" ]; then
        log "INFO" "File $file already exists, skipping download"
    else
        wget -c4 "$url" || {
            log "ERROR" "Failed to download $component from $url"
            return 1
        }
    fi
    
    return 0
}

# Function to check if running in a container
is_container() {
    # First check the environment variable set by the entrypoint script
    if [ "${IN_DOCKER_CONTAINER}" = "true" ]; then
        return 0
    fi
    
    # Fallback to traditional detection methods
    grep -q "docker\|container" /proc/1/cgroup 2>/dev/null || [ -f "/.dockerenv" ]
    return $?
}

# Function to extract using busybox tar (slowest method)
extract_with_busybox() {
    local file=$1
    local target=$2
    local strip=$3
    
    log "INFO" "Using busybox tar for extraction (slow method)"
    if [ "$strip" -gt 0 ]; then
        busybox tar -xf "$file" -C "$target" --strip-components=$strip
    else
        busybox tar -xf "$file" -C "$target"
    fi
    return $?
}

# Function to extract using pipe method with best available tools
extract_with_pipe() {
    local file=$1
    local target=$2
    local strip=$3
    
    log "INFO" "Using optimized pipe-based extraction"
    
    if [[ "$file" == *.tar.gz ]]; then
        # Use pigz for parallel gzip decompression if available
        if command -v pigz > /dev/null; then
            log "INFO" "Using pigz for parallel decompression (faster)"
            if [ "$strip" -gt 0 ]; then
                pigz -dc "$file" | tar -x -C "$target" --strip-components=$strip --no-same-owner
            else
                pigz -dc "$file" | tar -x -C "$target" --no-same-owner
            fi
        else
            # Fall back to regular gzip
            log "INFO" "Using standard gzip decompression"
            if [ "$strip" -gt 0 ]; then
                gzip -dc "$file" | tar -x -C "$target" --strip-components=$strip --no-same-owner
            else
                gzip -dc "$file" | tar -x -C "$target" --no-same-owner
            fi
        fi
    elif [[ "$file" == *.tar.xz ]]; then
        # Use XZ_OPT to enable parallel decompression for xz
        log "INFO" "Using parallel XZ decompression (XZ_OPT=-T0)"
        if [ "$strip" -gt 0 ]; then
            XZ_OPT="-T0" xz -dc "$file" | tar -x -C "$target" --strip-components=$strip --no-same-owner
        else
            XZ_OPT="-T0" xz -dc "$file" | tar -x -C "$target" --no-same-owner
        fi
    else
        return 1
    fi
    return $?
}

# Optimized extraction for large files like kernel source
extract_optimized() {
    local file=$1
    local target=$2
    local strip=$3
    local component=$4
    
    # Check if it's a large file that needs optimized extraction
    if [[ "$component" == "Linux kernel" ]]; then
        log "INFO" "Using optimized extraction for kernel source (large file)"
        
        # For kernel extraction, we'll optimize for performance
        if [[ "$file" == *.tar.xz ]]; then
            log "INFO" "Using parallel XZ extraction with minimal ownership changes"
            
            # Ensure target directory exists
            mkdir -p "$target"
            
            # Use XZ_OPT for parallel extraction and --no-same-owner to avoid slow ownership changes
            if [ "$strip" -gt 0 ]; then
                XZ_OPT="-T0" tar -xJf "$file" -C "$target" --strip-components=$strip --no-same-owner
            else
                XZ_OPT="-T0" tar -xJf "$file" -C "$target" --no-same-owner
            fi
            
            # Only make critical scripts executable
            if [ -f "$target/Makefile" ]; then
                chmod +x "$target/Makefile"
            fi
            
            # Make script directories executable
            for dir in scripts tools; do
                if [ -d "$target/$dir" ]; then
                    find "$target/$dir" -name "*.sh" -type f -exec chmod +x {} \; 2>/dev/null || true
                fi
            done
            
            return $?
        fi
    fi
    
    # If not a special case, return false to try other methods
    return 1
}

# Function to try cached version if extraction fails
try_cached_version() {
    local file=$1
    local target=$2
    local component=$3
    
    local base_name=$(basename "$file")
    local cache_dir="/onefilelinux/.buildcache/${component,,}"
    
    if [ -d "$cache_dir/${base_name%.tar.*}" ]; then
        log "INFO" "Found cached $component, copying"
        cp -a "$cache_dir/${base_name%.tar.*}/." "$target/"
        return $?
    fi
    
    return 1
}

# Function to extract an archive with container awareness
extract_archive() {
    local file=$1
    local target=$2
    local component=$3
    local strip_components=0
    
    log "INFO" "Extracting $component from $file"
    
    # If target is not specified, use current directory or component directory
    if [[ "$target" == "" ]]; then
        if [[ "$file" == *.tar.xz ]]; then
            target="${file%.tar.xz}"
            strip_components=1
        elif [[ "$file" == *.tar.gz ]]; then
            target="."
        fi
    fi
    
    # Check if target directory already exists and has a completion marker
    if [ -d "$target" ] && [ -f "$target/.extraction_complete" ]; then
        log "SUCCESS" "$component already extracted completely (found marker file)"
        return 0
    fi
    
    # Create target directory if it doesn't exist
    [ ! -d "$target" ] && mkdir -p "$target"
    
    # Determine extraction method based on environment
    if is_container; then
        log "INFO" "Detected container environment, using special extraction modes"
        
        # Check if extraction has been pre-handled by entrypoint script
        if [[ "$file" == *"alpine-minirootfs"* ]] && [ -d "./alpine-minirootfs" ] && [ -f "./alpine-minirootfs/etc/alpine-release" ]; then
            log "INFO" "Alpine minirootfs appears to be pre-extracted, skipping extraction"
            return 0
        fi
        
        # Try calling external handler if available (from entrypoint)
        if type handle_extraction &>/dev/null; then
            log "INFO" "Using entrypoint extraction handler"
            handle_extraction "$file" "$target" && return 0
        fi
        
        # Try extraction methods in order of efficiency
        
        # First try optimized extraction for specific components
        extract_optimized "$file" "$target" "$strip_components" "$component" && return 0
        
        # Next try pipe-based extraction (good balance of speed and compatibility)
        extract_with_pipe "$file" "$target" "$strip_components" && return 0
        
        # If pipe extraction fails, try using cached version
        if [ "${USE_CACHE:-false}" = "true" ]; then
            try_cached_version "$file" "$target" "$component" && return 0
        fi
        
        # Last resort - busybox tar (slowest but most compatible)
        if command -v busybox &> /dev/null; then
            extract_with_busybox "$file" "$target" "$strip_components" && return 0
        fi
        
        # Final attempt - try using sudo if available
        if command -v sudo &> /dev/null; then
            log "INFO" "Attempting extraction with sudo (privileged method)"
            
            # Create target directory
            sudo mkdir -p "$target"
            local success=1
            
            if [[ "$file" == *.tar.gz ]]; then
                # Use pigz if available
                if command -v pigz > /dev/null; then
                    log "INFO" "Using sudo with pigz for parallel decompression"
                    sudo sh -c "pigz -dc \"$file\" | tar -x -C \"$target\" --no-same-owner"
                    success=$?
                else
                    # Fall back to regular tar with gzip
                    sudo tar -xzf "$file" -C "$target" --no-same-owner
                    success=$?
                fi
            elif [[ "$file" == *.tar.xz ]]; then
                # Use parallel XZ decompression 
                log "INFO" "Using sudo with parallel XZ decompression"
                sudo sh -c "XZ_OPT='-T0' tar -xJf \"$file\" -C \"$target\" --strip-components=\"$strip_components\" --no-same-owner"
                success=$?
            fi
            
            # Handle permissions based on success and file type
            if [ $success -eq 0 ]; then
                if [[ "$component" == "Linux kernel" ]]; then
                    # For kernel, only make critical files executable
                    log "INFO" "Using targeted permission fixes for kernel source"
                    sudo find "$target/scripts" -name "*.sh" -type f -exec chmod +x {} \; 2>/dev/null || true
                    sudo chmod +x "$target/Makefile" 2>/dev/null || true
                else
                    # For other components, standard ownership change
                    sudo chown -R $(id -u):$(id -g) "$target"
                fi
                return 0
            fi
        fi
        
        log "ERROR" "All extraction methods failed for $file"
        return 1
    else
        # Standard extraction for non-container environments - using optimized methods
        log "INFO" "Using optimized extraction in standard environment"
        
        # Create target directory if it doesn't exist
        mkdir -p "$target"
        
        if [[ "$file" == *.tar.gz ]]; then
            # Use pigz for parallel decompression if available
            if command -v pigz > /dev/null; then
                log "INFO" "Using pigz for parallel decompression"
                pigz -dc "$file" | tar -x -C "$target" --no-same-owner || return 1
            else
                # Fall back to standard tar
                tar -C "$target" -xf "$file" || return 1
            fi
        elif [[ "$file" == *.tar.xz ]]; then
            # Use parallel XZ decompression
            log "INFO" "Using parallel XZ decompression"
            if [ "$strip_components" -gt 0 ]; then
                XZ_OPT="-T0" tar -xJf "$file" -C "$target" --strip-components=$strip_components --no-same-owner || return 1
            else
                XZ_OPT="-T0" tar -xJf "$file" -C "$target" --no-same-owner || return 1
            fi
        else
            log "ERROR" "Unknown archive format: $file"
            return 1
        fi
        
        # Special handling for Linux kernel
        if [[ "$component" == "Linux kernel" ]]; then
            log "INFO" "Using optimized permissions for kernel source"
            # Make critical scripts executable
            if [ -f "$target/Makefile" ]; then
                chmod +x "$target/Makefile"
            fi
            find "$target/scripts" -name "*.sh" -type f -exec chmod +x {} \; 2>/dev/null || true
        fi
    fi
    
    # Create a marker file to indicate successful extraction
    touch "$target/.extraction_complete"
    
    log "SUCCESS" "Extracted $component successfully"
    return 0
}

# Main function
main() {
    # Initialize script with standard header (prints banner)
    initialize_script
    
    # Step 1: Download and extract Alpine Linux
    start_timing "01_get: Alpine Linux"
    log "INFO" "Step 1: Getting Alpine Linux minirootfs"
    if ! download_and_verify "$ALPINE_URL" "Alpine Linux"; then
        log "ERROR" "Failed to download Alpine Linux"
        exit 1
    fi
    
    # Check if extraction is needed using our library function
    if needs_extraction "./alpine-minirootfs" "alpine"; then
        if ! extract_archive "$alpineminirootfsfile" "./alpine-minirootfs" "Alpine Linux"; then
            log "ERROR" "Failed to extract Alpine Linux"
            exit 1
        fi
        # Mark extraction as complete
        mark_extraction_complete "./alpine-minirootfs"
    else
        log "INFO" "Skipping Alpine Linux extraction - already completed"
    fi
    end_timing

    # Step 2: Download and extract Linux kernel
    start_timing "01_get: Linux kernel"
    log "INFO" "Step 2: Getting Linux kernel"
    if ! download_and_verify "$KERNEL_URL" "Linux kernel"; then
        log "ERROR" "Failed to download Linux kernel"
        exit 1
    fi
    
    # Check if extraction is needed using our library function
    if needs_extraction "$linuxver" "kernel"; then
        if ! extract_archive "$linuxver.tar.xz" "" "Linux kernel"; then
            log "ERROR" "Failed to extract Linux kernel"
            exit 1
        fi
        # Mark extraction as complete
        mark_extraction_complete "$linuxver"
    else
        log "INFO" "Skipping Linux kernel extraction - already completed"
    fi

    # Create symbolic link to Linux kernel directory
    if [ ! -L "linux" ] || [ ! -d "linux" ]; then
        ln -sf "$linuxver" linux
        log "INFO" "Created symbolic link to Linux kernel directory"
    fi
    end_timing

    # Step 3: OpenZFS is now handled via Alpine packages
    if [ "${INCLUDE_ZFS:-true}" = "true" ]; then
        log "INFO" "Step 3: Using Alpine ZFS packages instead of building from source"
        log "INFO" "Skipping OpenZFS source download and extraction"
    else
        log "INFO" "Step 3: Skipping OpenZFS (disabled in configuration)"
    fi

    # Print final status
    print_script_end
    
    # If this is the final script being run, finalize the timing log
    if [ "${FINALIZE_TIMING_LOG:-false}" = "true" ]; then
        finalize_timing_log
    fi
    
    return 0
}

# Execute main function
main "$@"

