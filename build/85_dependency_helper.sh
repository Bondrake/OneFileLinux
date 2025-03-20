#!/bin/bash
#
# OneFileLinux Dependency Helper (85_dependency_helper.sh)
# Provides consistent dependency management across all build environments
# This is part of the library scripts (80-89 range)
#

# Define common package groups that can be installed consistently
# across different package managers and build environments

# Array of basic build tools required for all environments
declare -a BASIC_BUILD_TOOLS=(
    "wget"
    "tar"
    "xz-utils" # May be "xz" on some distros
    "build-essential" # May be "gcc make" on some distros
    "flex"
    "bison"
    "bc"
    "kmod"
    "libelf-dev" # May be "elfutils-dev" on some distros
)

# Array of compression tools
declare -a COMPRESSION_TOOLS=(
    "xz-utils" # May be "xz" on some distros
    "lzma"
    "zstd"
    "upx-ucl" # May be "upx" on some distros
)

# Array of build performance tools
declare -a BUILD_PERFORMANCE_TOOLS=(
    "ccache"
)

# Array of crypto/SSL tools
declare -a CRYPTO_TOOLS=(
    "libssl-dev" # May be "openssl-dev" on some distros
)

# Array of kernel build dependencies
declare -a KERNEL_DEPS=(
    "libncurses-dev" # May be "ncurses-dev" on some distros
    "libelf-dev"     # May be "elfutils-dev" on some distros
    "python3"        # For kernel scripts
)

# Array of EFI build dependencies
declare -a EFI_DEPS=(
    "mtools"
    "xorriso" 
)

# Array of ZFS dependencies
declare -a ZFS_DEPS=(
    "util-linux-dev"  # May be different on some distros
    "libuuid-dev"     # May be "libuuid" on some distros
    "libattr-dev"     # May be "attr-dev" on some distros
    "libblkid-dev"    # May be part of util-linux-dev on some distros
    "zlib1g-dev"      # May be "zlib-dev" on some distros
    "libudev-dev"     # May be "eudev-dev" on some distros
)

# Map OS names to package manager commands
# This allows for consistent package installation across different distros
get_package_manager_install_cmd() {
    local os_name=$1
    
    case "$os_name" in
        "Ubuntu"|"Debian"|"Pop!_OS"|"Linux Mint")
            echo "apt-get update && apt-get install -y"
            ;;
        "Fedora")
            echo "dnf install -y"
            ;;
        "CentOS"|"Red Hat Enterprise Linux"|"RHEL")
            echo "yum install -y"
            ;;
        "Arch Linux"|"Manjaro Linux")
            echo "pacman -Sy --noconfirm"
            ;;
        "Alpine Linux")
            echo "apk add"
            ;;
        *)
            echo ""
            ;;
    esac
}

# Map package names between different distros
# This returns the correct package name for the given OS
map_package_name() {
    local os_name=$1
    local generic_name=$2
    
    case "$generic_name:$os_name" in
        "xz-utils:Alpine Linux")
            echo "xz"
            ;;
        "xz-utils:Arch Linux"|"xz-utils:Manjaro Linux")
            echo "xz"
            ;;
        "xz-utils:Fedora"|"xz-utils:CentOS"|"xz-utils:Red Hat Enterprise Linux"|"xz-utils:RHEL")
            echo "xz"
            ;;
        "build-essential:Alpine Linux")
            echo "build-base"
            ;;
        "build-essential:Arch Linux"|"build-essential:Manjaro Linux")
            echo "base-devel"
            ;;
        "build-essential:Fedora"|"build-essential:CentOS"|"build-essential:Red Hat Enterprise Linux"|"build-essential:RHEL")
            echo "gcc make"
            ;;
        "libelf-dev:Alpine Linux")
            echo "elfutils-dev"
            ;;
        "libelf-dev:Arch Linux"|"libelf-dev:Manjaro Linux")
            echo "libelf"
            ;;
        "libelf-dev:Fedora"|"libelf-dev:CentOS"|"libelf-dev:Red Hat Enterprise Linux"|"libelf-dev:RHEL")
            echo "elfutils-libelf-devel"
            ;;
        "libssl-dev:Alpine Linux")
            echo "openssl-dev"
            ;;
        "libssl-dev:Arch Linux"|"libssl-dev:Manjaro Linux")
            echo "openssl"
            ;;
        "libssl-dev:Fedora"|"libssl-dev:CentOS"|"libssl-dev:Red Hat Enterprise Linux"|"libssl-dev:RHEL")
            echo "openssl-devel"
            ;;
        "upx-ucl:Alpine Linux"|"upx-ucl:Arch Linux"|"upx-ucl:Manjaro Linux"|"upx-ucl:Fedora"|"upx-ucl:CentOS"|"upx-ucl:Red Hat Enterprise Linux"|"upx-ucl:RHEL")
            echo "upx"
            ;;
        "libncurses-dev:Alpine Linux")
            echo "ncurses-dev"
            ;;
        "libncurses-dev:Arch Linux"|"libncurses-dev:Manjaro Linux")
            echo "ncurses"
            ;;
        "libncurses-dev:Fedora"|"libncurses-dev:CentOS"|"libncurses-dev:Red Hat Enterprise Linux"|"libncurses-dev:RHEL")
            echo "ncurses-devel"
            ;;
        "libuuid-dev:Alpine Linux")
            echo "libuuid"
            ;;
        "libattr-dev:Alpine Linux")
            echo "attr-dev"
            ;;
        "libudev-dev:Alpine Linux")
            echo "eudev-dev"
            ;;
        *)
            # Default: return the generic name if no mapping exists
            echo "$generic_name"
            ;;
    esac
}

# Get a list of all dependencies for a given OS, combining multiple package groups
# This provides the complete list of packages needed for the build
get_dependencies_for_os() {
    local os_name=$1
    local include_zfs=${2:-true}
    local include_efi=${3:-true}
    
    # Start with an empty list of packages
    local packages=""
    
    # Add basic build tools
    for pkg in "${BASIC_BUILD_TOOLS[@]}"; do
        packages="$packages $(map_package_name "$os_name" "$pkg")"
    done
    
    # Add compression tools
    for pkg in "${COMPRESSION_TOOLS[@]}"; do
        packages="$packages $(map_package_name "$os_name" "$pkg")"
    done
    
    # Add build performance tools
    for pkg in "${BUILD_PERFORMANCE_TOOLS[@]}"; do
        packages="$packages $(map_package_name "$os_name" "$pkg")"
    done
    
    # Add crypto tools
    for pkg in "${CRYPTO_TOOLS[@]}"; do
        packages="$packages $(map_package_name "$os_name" "$pkg")"
    done
    
    # Add kernel build dependencies
    for pkg in "${KERNEL_DEPS[@]}"; do
        packages="$packages $(map_package_name "$os_name" "$pkg")"
    done
    
    # Add EFI dependencies if requested
    if [ "$include_efi" = "true" ]; then
        for pkg in "${EFI_DEPS[@]}"; do
            packages="$packages $(map_package_name "$os_name" "$pkg")"
        done
    fi
    
    # Add ZFS dependencies if requested
    if [ "$include_zfs" = "true" ]; then
        for pkg in "${ZFS_DEPS[@]}"; do
            packages="$packages $(map_package_name "$os_name" "$pkg")"
        done
    fi
    
    # Return the list of packages
    echo "$packages"
}

# Get Docker dependencies list
get_docker_dependencies() {
    local os_name="Alpine Linux"
    get_dependencies_for_os "$os_name" true true
}

# Get the install command for the current OS
# Can be used to generate consistent install commands across different distros
get_os_install_command() {
    # Detect OS and version
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        OS=$NAME
        VER=$VERSION_ID
    elif [ -f /etc/debian_version ]; then
        OS="Debian"
        VER=$(cat /etc/debian_version)
    elif [ -f /etc/redhat-release ]; then
        OS=$(cat /etc/redhat-release | cut -d ' ' -f 1)
        VER=$(cat /etc/redhat-release | grep -oE '[0-9]+\.[0-9]+')
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        OS="macOS"
        VER=$(sw_vers -productVersion)
    else
        OS="Unknown"
        VER="Unknown"
    fi
    
    # Get the package manager install command
    local install_cmd=$(get_package_manager_install_cmd "$OS")
    
    # Get the list of dependencies for this OS
    local packages=$(get_dependencies_for_os "$OS" "${INCLUDE_ZFS:-true}" "true")
    
    # Return the full install command
    if [ -n "$install_cmd" ]; then
        echo "$install_cmd $packages"
    else
        echo "# No package manager found for $OS"
        echo "# Manual installation required for: $packages"
    fi
}

# Update the list of dependencies in the Dockerfile
# This ensures that the Docker and local builds use the same dependencies
update_dockerfile_dependencies() {
    local dockerfile_path=$1
    
    if [ ! -f "$dockerfile_path" ]; then
        echo "Error: Dockerfile not found at $dockerfile_path"
        return 1
    fi
    
    # Get the dependencies for Docker (Alpine Linux)
    local dependencies=$(get_docker_dependencies)
    
    # Build the RUN command for installing dependencies
    local run_cmd="RUN apk add --no-cache \\\\"
    for pkg in $dependencies; do
        run_cmd="${run_cmd}\n    $pkg \\\\"
    done
    # Remove the last trailing backslash
    run_cmd="${run_cmd%\\\\}"
    
    # Replace the RUN command in the Dockerfile
    sed -i -e '/^RUN apk add --no-cache/,/^[[:space:]]*[^[:space:]]/c\'"$run_cmd" "$dockerfile_path"
    
    echo "Updated Docker dependencies in $dockerfile_path"
    return 0
}

# Export all functions
export -f get_package_manager_install_cmd
export -f map_package_name
export -f get_dependencies_for_os
export -f get_docker_dependencies
export -f get_os_install_command
export -f update_dockerfile_dependencies