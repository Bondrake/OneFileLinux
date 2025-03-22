#!/bin/bash
# OneFileLinux minimal bootstrapper
# This script handles dependencies and provides build options
# It now includes direct support for dry-run mode

set -e  # Exit on error

# Load configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
CONFIG_FILE="${SCRIPT_DIR}/build-config.sh"

if [[ ! -f "$CONFIG_FILE" ]]; then
    echo "ERROR: Configuration file not found: $CONFIG_FILE"
    exit 1
fi

source "$CONFIG_FILE"

PROJECT_ROOT="$(pwd)"
BUILD_MODE=""
DRY_RUN=""
DRY_RUN_UNTIL=""
INSTALL_SBCL_FROM_SOURCE=false
UPGRADE_SBCL=false

# Process command line arguments
process_args() {
    CONTAINER_ENGINE="$DEFAULT_CONTAINER_ENGINE"
    
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --local)
                BUILD_MODE="local"
                shift
                ;;
            --docker)
                BUILD_MODE="docker"
                CONTAINER_ENGINE="docker"
                shift
                ;;
            --podman)
                BUILD_MODE="container"
                CONTAINER_ENGINE="podman"
                shift
                ;;
            --container-engine)
                if [[ -z "$2" ]]; then
                    echo "Error: --container-engine requires an argument"
                    exit 1
                fi
                CONTAINER_ENGINE="$2"
                shift 2
                ;;
            --all)
                BUILD_MODE="all"
                shift
                ;;
            --sbcl-from-source)
                INSTALL_SBCL_FROM_SOURCE=true
                shift
                ;;
            --sbcl-upgrade)
                UPGRADE_SBCL=true
                shift
                ;;
            --help|-h)
                echo "OneFileLinux Build Environment Setup"
                echo ""
                echo "Usage: $0 [options]"
                echo ""
                echo "Options:"
                echo "  --local              Setup for local SBCL build only"
                echo "  --docker             Setup for Docker build only"
                echo "  --podman             Setup for Podman build only"
                echo "  --container-engine ENGINE  Specify container engine (docker or podman)"
                echo "  --all                Setup for both local and container builds (default: $DEFAULT_CONTAINER_ENGINE)" 
                echo "  --sbcl-from-source   Install latest SBCL from source (Ubuntu/Debian only)"
                echo "  --sbcl-upgrade       Upgrade existing SBCL installation to latest version (Ubuntu/Debian only)"
                echo "  --help, -h           Display this help message"
                echo ""
                echo "Examples:"
                echo "  $0 --all                      # Setup both local and $DEFAULT_CONTAINER_ENGINE"
                echo "  $0 --all --container-engine podman  # Setup both local and podman"
                echo ""
                exit 0
                ;;
            *)
                echo "Unknown option: $1"
                echo "Use --help to see available options"
                exit 1
                ;;
        esac
    done
}

# Prompt user for build mode if not specified
prompt_build_mode() {
    if [[ -z "$BUILD_MODE" ]]; then
        # Adapt options based on OS
        if [[ "$OS_TYPE" != "linux" ]]; then
            # Non-Linux platforms only support container builds
            echo "On ${OS_TYPE^}, container-based builds are the only supported method."
            
            # Check for container engines
            if check_docker || check_podman; then
                # Offer choice between available container engines
                if check_docker && check_podman; then
                    echo ""
                    echo "Both Docker and Podman are available on your system."
                    echo "Which container engine would you prefer to use?"
                    echo "1) Docker"
                    echo "2) Podman"
                    echo ""
                    read -p "Enter option [1-2]: " container_choice
                    
                    case "$container_choice" in
                        1) CONTAINER_ENGINE="docker" ;;
                        2) CONTAINER_ENGINE="podman" ;;
                        *)
                            echo "Invalid choice. Defaulting to '$DEFAULT_CONTAINER_ENGINE'"
                            CONTAINER_ENGINE="$DEFAULT_CONTAINER_ENGINE"
                            ;;
                    esac
                elif check_podman; then
                    CONTAINER_ENGINE="podman"
                    echo "Using Podman as the container engine"
                else
                    CONTAINER_ENGINE="docker"
                    echo "Using Docker as the container engine"
                fi
                BUILD_MODE="container"
            else
                echo "No container engines (Docker/Podman) detected. Please install one to continue."
                exit 1
            fi
        else
            # Linux platforms support all build methods
            echo "How would you like to build OneFileLinux?"
            echo "1) Local build with SBCL (requires local dependencies)"
            echo "2) Container build (recommended for isolated builds)"
            echo "3) Setup both options"
            echo ""
            read -p "Enter option [1-3]: " choice
            
            case "$choice" in
                1) BUILD_MODE="local" ;;
                2) 
                    # Offer choice between Docker and Podman
                    if check_docker && check_podman; then
                        echo ""
                        echo "Both Docker and Podman are available on your system."
                        echo "Which container engine would you prefer to use?"
                        echo "1) Docker"
                        echo "2) Podman"
                        echo ""
                        read -p "Enter option [1-2]: " container_choice
                        
                        case "$container_choice" in
                            1) CONTAINER_ENGINE="docker" ;;
                            2) CONTAINER_ENGINE="podman" ;;
                            *)
                                echo "Invalid choice. Defaulting to '$DEFAULT_CONTAINER_ENGINE'"
                                CONTAINER_ENGINE="$DEFAULT_CONTAINER_ENGINE"
                                ;;
                        esac
                    elif check_podman; then
                        CONTAINER_ENGINE="podman"
                        echo "Using Podman as the container engine"
                    else
                        CONTAINER_ENGINE="docker"
                        echo "Using Docker as the container engine"
                    fi
                    BUILD_MODE="container"
                    ;;
                3) BUILD_MODE="all" ;;
                *)
                    echo "Invalid choice. Defaulting to 'all'"
                    BUILD_MODE="all"
                    ;;
            esac
        fi
    fi
}

# Detect OS and distribution
detect_distro() {
    # First, detect basic OS type
    case "$(uname -s)" in
        Linux*)
            OS_TYPE="${OS_TYPES[0]}"  # linux
            OS_FAMILY="${OS_FAMILIES[0]}"  # linux
            ;;
        Darwin*)
            OS_TYPE="${OS_TYPES[1]}"  # macos
            OS_FAMILY="${OS_FAMILIES[1]}"  # darwin
            DISTRO_ID="macos"
            DISTRO_NAME="macOS"
            DISTRO_VERSION=$(sw_vers -productVersion 2>/dev/null || echo "unknown")
            DISTRO_FAMILY="macos"
            echo "Detected OS: $DISTRO_NAME $DISTRO_VERSION"
            echo "$MACOS_LIMITED_MSG"
            return
            ;;
        CYGWIN*|MINGW*|MSYS*)
            OS_TYPE="${OS_TYPES[2]}"  # windows
            OS_FAMILY="${OS_FAMILIES[2]}"  # windows
            DISTRO_ID="windows"
            DISTRO_NAME="Windows"
            DISTRO_VERSION=$(uname -r)
            DISTRO_FAMILY="windows"
            echo "Detected OS: $DISTRO_NAME $DISTRO_VERSION"
            echo "$WINDOWS_LIMITED_MSG"
            return
            ;;
        *)
            OS_TYPE="${OS_TYPES[3]}"  # unknown
            OS_FAMILY="${OS_FAMILIES[3]}"  # unknown
            echo "$UNKNOWN_OS_MSG: $(uname -s)"
            ;;
    esac
    
    # For Linux, get distribution details
    if [[ "$OS_TYPE" == "linux" ]]; then
        if [ -f "$DEBIAN_OS_RELEASE" ]; then
            # freedesktop.org and systemd
            . "$DEBIAN_OS_RELEASE"
            DISTRO_ID=$ID
            DISTRO_FAMILY=$ID_LIKE
            DISTRO_NAME=$NAME
            DISTRO_VERSION=$VERSION_ID
        elif type lsb_release >/dev/null 2>&1; then
            # linuxbase.org
            DISTRO_ID=$(lsb_release -si | tr '[:upper:]' '[:lower:]')
            DISTRO_NAME=$(lsb_release -sd)
            DISTRO_VERSION=$(lsb_release -sr)
        elif [ -f "$LSB_RELEASE_FILE" ]; then
            # For some versions of Debian/Ubuntu without lsb_release command
            . "$LSB_RELEASE_FILE"
            DISTRO_ID=$DISTRIB_ID
            DISTRO_NAME=$DISTRIB_DESCRIPTION
            DISTRO_VERSION=$DISTRIB_RELEASE
        elif [ -f "$DEBIAN_VERSION_FILE" ]; then
            # Older Debian/Ubuntu/etc.
            DISTRO_ID="debian"
            DISTRO_NAME="Debian"
            DISTRO_VERSION=$(cat "$DEBIAN_VERSION_FILE")
        elif [ -f "$REDHAT_RELEASE_FILE" ]; then
            # Older Red Hat, CentOS, etc.
            DISTRO_NAME=$(cat "$REDHAT_RELEASE_FILE")
            if grep -q "CentOS" "$REDHAT_RELEASE_FILE"; then
                DISTRO_ID="centos"
            else
                DISTRO_ID="rhel"
            fi
        elif [ -f "$ALPINE_RELEASE_FILE" ]; then
            # Alpine
            DISTRO_ID="alpine"
            DISTRO_NAME="Alpine Linux"
            DISTRO_VERSION=$(cat "$ALPINE_RELEASE_FILE")
        else
            # Fall back to uname
            DISTRO_ID="linux"
            DISTRO_NAME="Linux"
            DISTRO_VERSION=$(uname -r)
        fi
        
        # Determine distribution family for Linux
        if [[ -n "${DISTRO_FAMILY_MAP[$DISTRO_ID]}" ]]; then
            DISTRO_FAMILY="${DISTRO_FAMILY_MAP[$DISTRO_ID]}"
        else
            DISTRO_FAMILY="$DISTRO_ID"
        fi
        
        echo "Detected distribution: $DISTRO_NAME $DISTRO_VERSION (Family: $DISTRO_FAMILY)"
    fi
}

# Check for SBCL installation
check_sbcl() {
    if command -v sbcl >/dev/null 2>&1; then
        return 0
    else
        return 1
    fi
}

# Install SBCL from source
install_sbcl_from_source() {
    echo "Installing SBCL from source (latest version)..."
    
    # Install dependencies for building SBCL
    case "$DISTRO_FAMILY" in
        debian)
            echo "Installing build dependencies on Debian-based distribution..."
            sudo apt-get update
            sudo apt-get install -y git build-essential zlib1g-dev libzstd-dev texinfo
            
            # Check for existing SBCL for bootstrapping
            # If not available, install the packaged version temporarily
            if ! command -v sbcl >/dev/null 2>&1; then
                echo "Installing packaged SBCL for bootstrapping..."
                sudo apt-get install -y sbcl
            fi
            ;;
        *)
            echo "Source-based installation is currently only supported on Debian-based systems."
            return 1
            ;;
    esac
    
    # Create a temporary build directory
    SBCL_BUILD_DIR=$(mktemp -d)
    echo "Building in temporary directory: $SBCL_BUILD_DIR"
    cd "$SBCL_BUILD_DIR" || { 
        echo "Failed to change to build directory"; 
        return 1; 
    }
    
    # Clone the latest SBCL source
    echo "Cloning latest SBCL source from GitHub..."
    if ! git clone --depth 1 https://github.com/sbcl/sbcl.git; then
        echo "Failed to clone SBCL repository"
        cd "$PROJECT_ROOT" || true
        rm -rf "$SBCL_BUILD_DIR"
        return 1
    fi
    
    cd sbcl || {
        echo "Failed to change to SBCL source directory"
        cd "$PROJECT_ROOT" || true
        rm -rf "$SBCL_BUILD_DIR"
        return 1
    }
    
    # Create version.lisp-expr manually if git describe fails
    if [ ! -f "version.lisp-expr" ]; then
        echo "Creating version file manually..."
        # Use a hardcoded version string that will be overridden during install
        echo '"2.4.0.999-git"' > version.lisp-expr
    fi
    
    # Build SBCL
    echo "Building SBCL (this may take several minutes)..."
    if ! sh make.sh --fancy; then
        echo "SBCL build failed"
        cd "$PROJECT_ROOT" || true
        rm -rf "$SBCL_BUILD_DIR"
        return 1
    fi
    
    # Verify build successful
    if [ ! -f "src/runtime/sbcl" ]; then
        echo "SBCL build did not produce the expected binary"
        cd "$PROJECT_ROOT" || true
        rm -rf "$SBCL_BUILD_DIR"
        return 1
    fi
    
    # Install SBCL
    echo "Installing SBCL system-wide..."
    if ! sudo sh install.sh; then
        echo "SBCL installation failed"
        cd "$PROJECT_ROOT" || true
        rm -rf "$SBCL_BUILD_DIR"
        return 1
    fi
    
    # Clean up
    cd "$PROJECT_ROOT" || true
    rm -rf "$SBCL_BUILD_DIR"
    
    # Verify installation
    if command -v sbcl >/dev/null 2>&1; then
        SBCL_VERSION=$(sbcl --version)
        echo "SBCL installation successful: $SBCL_VERSION"
        return 0
    else
        echo "SBCL installation failed: executable not found in PATH"
        return 1
    fi
}

# Install SBCL and other dependencies based on distribution
install_sbcl() {
    echo "SBCL not found. Installing Steel Bank Common Lisp..."
    
    case "$DISTRO_FAMILY" in
        debian)
            if [[ "$INSTALL_SBCL_FROM_SOURCE" == "true" ]]; then
                echo "Installing SBCL from source on Debian-based distribution..."
                install_sbcl_from_source
                # Install additional dependencies
                sudo apt-get update && sudo apt-get install -y curl build-essential
            else
                echo "Installing SBCL on Debian-based distribution..."
                sudo apt-get update && sudo apt-get install -y ${DEBIAN_PACKAGES[@]}
            fi
            ;;
        fedora|rhel)
            echo "Installing SBCL on Red Hat-based distribution..."
            if command -v dnf >/dev/null 2>&1; then
                sudo dnf install -y ${REDHAT_DNF_PACKAGES[@]}
            else
                sudo yum install -y ${REDHAT_YUM_PACKAGES[@]}
            fi
            ;;
        arch)
            echo "Installing SBCL on Arch-based distribution..."
            sudo pacman -Sy --noconfirm ${ARCH_PACKAGES[@]}
            ;;
        suse)
            echo "Installing SBCL on SUSE-based distribution..."
            sudo zypper install -y ${SUSE_PACKAGES[@]}
            ;;
        alpine)
            echo "Installing SBCL on Alpine Linux..."
            sudo apk add --no-cache ${ALPINE_PACKAGES[@]}
            ;;
        gentoo)
            echo "Installing SBCL on Gentoo Linux..."
            sudo emerge --ask ${GENTOO_PACKAGES[@]}
            ;;
        void)
            echo "Installing SBCL on Void Linux..."
            sudo xbps-install -Sy ${VOID_PACKAGES[@]}
            ;;
        macos)
            echo "Installing SBCL on macOS..."
            if command -v brew >/dev/null 2>&1; then
                brew install ${MACOS_PACKAGES[@]}
            else
                echo "Homebrew not found. Please install Homebrew first: https://brew.sh/"
                exit 1
            fi
            ;;
        *)
            echo "Unknown distribution. Please install SBCL manually."
            echo "For most distributions, you can use your package manager to install 'sbcl'."
            exit 1
            ;;
    esac
}

# Check for Common Lisp libraries
check_quicklisp() {
    # Create a more thorough Quicklisp check script
    cat > /tmp/check-quicklisp.lisp << 'EOF'
(require :asdf)

;; Check for setup.lisp existence
(let ((ql-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (if (probe-file ql-setup)
      (handler-case
          (progn
            ;; Try to actually load and use Quicklisp
            (load ql-setup)
            (when (find-package :quicklisp)
              ;; Try to execute a basic Quicklisp function
              (funcall (read-from-string "ql:system-apropos") "alexandria")
              ;; If we get here, it worked
              (format t "Quicklisp is properly installed and functional.~%")
              (quit :unix-status 0)))
        (error (e)
          (format t "Quicklisp exists but failed to load properly: ~A~%" e)
          (quit :unix-status 2)))
      (progn
        (format t "Quicklisp setup.lisp not found.~%")
        (quit :unix-status 1))))
EOF

    # Run the check script
    sbcl --noinform --non-interactive --load /tmp/check-quicklisp.lisp
    local status=$?
    
    # Cleanup
    rm /tmp/check-quicklisp.lisp
    
    # Return appropriate status
    if [ $status -eq 0 ]; then
        return 0  # Properly installed and working
    else
        return 1  # Not installed or not working
    fi
}

install_quicklisp() {
    echo "Installing Quicklisp for library dependencies..."
    
    # Check if quicklisp is already installed but the check failed
    if [ -d "$HOME/quicklisp" ]; then
        echo "Quicklisp directory exists but may be incomplete. Repairing installation..."
        
        # Create a repair script that handles already-installed case
        cat > /tmp/repair-quicklisp.lisp << 'EOF'
(let ((ql-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (if (probe-file ql-setup)
      (progn
        (format t "~%Found existing Quicklisp at ~A, loading it...~%" ql-setup)
        (load ql-setup)
        (format t "~%Adding Quicklisp to SBCL init file...~%")
        (funcall (read-from-string "ql:add-to-init-file"))
        (format t "~%Quicklisp successfully configured.~%"))
      (progn
        (format t "~%Installing fresh Quicklisp...~%")
        (load "/tmp/quicklisp.lisp")
        (funcall (read-from-string "quicklisp-quickstart:install"))
        (funcall (read-from-string "ql:add-to-init-file"))
        (format t "~%Quicklisp installation complete.~%"))))
(quit)
EOF
        
        # Download Quicklisp (for fresh install case)
        curl -s -o /tmp/quicklisp.lisp "$QUICKLISP_URL"
        
        # Run the repair script
        sbcl --noinform --non-interactive --load /tmp/repair-quicklisp.lisp
        
        # Cleanup
        rm /tmp/repair-quicklisp.lisp
        rm /tmp/quicklisp.lisp
        
        return 0
    fi
    
    # Fresh installation
    # Download Quicklisp
    curl -s -o /tmp/quicklisp.lisp "$QUICKLISP_URL"
    
    # Install Quicklisp
    echo "Performing fresh Quicklisp installation..."
    sbcl --noinform --non-interactive \
        --load /tmp/quicklisp.lisp \
        --eval '(handler-case (quicklisp-quickstart:install) (error (e) (format t "~%Note: ~A~%This is normal if Quicklisp is already installed.~%" e) (values)))' \
        --eval '(handler-case (ql:add-to-init-file) (error (e) (format t "~%Could not add to init file: ~A~%" e) (values)))' \
        --eval '(quit)'
    
    # Cleanup
    rm /tmp/quicklisp.lisp
}

# Install required libraries
install_dependencies() {
    echo "Installing required Common Lisp libraries..."
    
    # Create a more resilient dependency loader script
    cat > /tmp/load-dependencies.lisp << 'EOF'
(require :asdf)

;; Try to load Quicklisp
(let ((ql-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (if (probe-file ql-setup)
      (progn
        (format t "~%Loading Quicklisp from ~A...~%" ql-setup)
        (load ql-setup)
        (format t "Quicklisp loaded successfully.~%"))
      (error "Quicklisp setup not found at ~A. Please reinstall Quicklisp."
             ql-setup)))

;; Set verbose mode
(when (find-package :quicklisp)
  (setf (symbol-value (read-from-string "ql:*quickload-verbose*")) t))

;; Define dependency loading function with error handling
(defun safe-load-dependency (lib-name)
  (format t "~%Loading dependency: ~A~%" lib-name)
  (handler-case
      (progn
        (funcall (read-from-string "ql:quickload") lib-name)
        (format t "~A loaded successfully~%" lib-name)
        t)
    (error (e)
      (format t "~%Warning: Failed to load ~A: ~A~%" lib-name e)
      nil)))

;; Load each dependency with proper error handling
(defvar *loaded-count* 0)
(defvar *failed-count* 0)
(defvar *lib-list* '(:uiop :cl-ppcre :alexandria))

(dolist (lib *lib-list*)
  (if (safe-load-dependency lib)
      (incf *loaded-count*)
      (incf *failed-count*)))

;; Report results
(format t "~%~%Dependency loading complete:~%")
(format t "  ~D packages loaded successfully~%" *loaded-count*)
(format t "  ~D packages failed to load~%" *failed-count*)

(if (zerop *failed-count*)
    (format t "~%All dependencies loaded successfully.~%")
    (format t "~%Warning: Some dependencies failed to load.~%"))

(quit)
EOF

    # Execute the dependency loader script
    echo "Loading dependencies: ${CL_LIBS[*]}"
    sbcl --noinform --non-interactive --load /tmp/load-dependencies.lisp
    
    # Cleanup
    rm /tmp/load-dependencies.lisp
}

# Check Docker installation
check_docker() {
    if command -v docker >/dev/null 2>&1; then
        return 0
    else
        return 1
    fi
}

# Check Podman installation
check_podman() {
    if command -v podman >/dev/null 2>&1; then
        return 0
    else
        return 1
    fi
}

# Check for any container engine
check_container_engine() {
    if check_docker || check_podman; then
        return 0
    else
        return 1
    fi
}

# Suggest Docker installation based on distro
suggest_docker_install() {
    echo "Docker not found. Installation instructions for your system:"
    
    # Replace any DISTRO_ID placeholders in commands
    local REPLACE_ID="s/DISTRO_ID/$DISTRO_ID/g"
    
    case "$DISTRO_FAMILY" in
        debian)
            for cmd in "${DEBIAN_DOCKER_INSTALL[@]}"; do
                echo "$(echo "$cmd" | sed "$REPLACE_ID")"
            done
            echo ""
            echo "After installing, either:"
            echo "1. Run 'newgrp docker' to use Docker without logging out"
            echo "2. Log out and log back in to apply group changes"
            ;;
        fedora)
            for cmd in "${FEDORA_DOCKER_INSTALL[@]}"; do
                echo "$cmd"
            done
            echo ""
            echo "After installing, either:"
            echo "1. Run 'newgrp docker' to use Docker without logging out"
            echo "2. Log out and log back in to apply group changes"
            ;;
        rhel)
            for cmd in "${RHEL_DOCKER_INSTALL[@]}"; do
                echo "$cmd"
            done
            echo ""
            echo "After installing, either:"
            echo "1. Run 'newgrp docker' to use Docker without logging out"
            echo "2. Log out and log back in to apply group changes"
            ;;
        arch)
            for cmd in "${ARCH_DOCKER_INSTALL[@]}"; do
                echo "$cmd"
            done
            echo ""
            echo "After installing, either:"
            echo "1. Run 'newgrp docker' to use Docker without logging out"
            echo "2. Log out and log back in to apply group changes"
            ;;
        suse)
            for cmd in "${SUSE_DOCKER_INSTALL[@]}"; do
                echo "$cmd"
            done
            echo ""
            echo "After installing, either:"
            echo "1. Run 'newgrp docker' to use Docker without logging out"
            echo "2. Log out and log back in to apply group changes"
            ;;
        alpine)
            for cmd in "${ALPINE_DOCKER_INSTALL[@]}"; do
                echo "$cmd"
            done
            echo ""
            echo "After installing, either:"
            echo "1. Run 'newgrp docker' to use Docker without logging out"
            echo "2. Log out and log back in to apply group changes"
            ;;
        macos)
            echo "Please download and install Docker Desktop from:"
            echo "$DOCKER_DESKTOP_URL"
            ;;
        *)
            echo "Please visit $DOCKER_DOCS_URL for Docker installation instructions"
            ;;
    esac
}

# Suggest Podman installation based on distro
suggest_podman_install() {
    echo "Podman not found. Installation instructions for your system:"
    
    case "$DISTRO_FAMILY" in
        debian)
            for cmd in "${DEBIAN_PODMAN_INSTALL[@]}"; do
                echo "$cmd"
            done
            ;;
        fedora)
            for cmd in "${FEDORA_PODMAN_INSTALL[@]}"; do
                echo "$cmd"
            done
            ;;
        rhel)
            for cmd in "${RHEL_PODMAN_INSTALL[@]}"; do
                echo "$cmd"
            done
            ;;
        arch)
            for cmd in "${ARCH_PODMAN_INSTALL[@]}"; do
                echo "$cmd"
            done
            ;;
        suse)
            for cmd in "${SUSE_PODMAN_INSTALL[@]}"; do
                echo "$cmd"
            done
            ;;
        alpine)
            for cmd in "${ALPINE_PODMAN_INSTALL[@]}"; do
                echo "$cmd"
            done
            ;;
        macos)
            echo "Podman on macOS requires a VM. You can install podman via Homebrew:"
            echo "brew install podman"
            echo "podman machine init"
            echo "podman machine start"
            ;;
        *)
            echo "Please visit $PODMAN_DOCS_URL for Podman installation instructions"
            ;;
    esac
}

# Setup for local build
setup_local_build() {
    # Check if running on a non-Linux platform
    if [[ "$OS_TYPE" != "linux" ]]; then
        echo "Direct SBCL builds are only supported on Linux."
        echo "For ${OS_TYPE^} users, container-based builds are the only supported method."
        
        # Ask if user wants to continue with container setup instead
        read -p "Would you like to set up for container-based build instead? [Y/n] " use_container
        if [[ "$use_container" != "n" && "$use_container" != "N" ]]; then
            setup_container_build "$DEFAULT_CONTAINER_ENGINE"
            return $?
        else
            return 1
        fi
    fi
    
    echo "Setting up for local SBCL build..."
    
    # Check and install/upgrade SBCL
    if ! check_sbcl; then
        install_sbcl
    elif [[ "$UPGRADE_SBCL" == "true" ]]; then
        echo "Upgrading existing SBCL installation..."
        if [[ "$DISTRO_FAMILY" == "debian" ]]; then
            install_sbcl_from_source
        else
            echo "SBCL upgrade is only supported on Debian-based systems."
        fi
    else
        echo "SBCL already installed."
    fi
    
    # Check and install Quicklisp
    if ! check_quicklisp; then
        install_quicklisp
    else
        echo "Quicklisp already installed."
    fi
    
    # Install dependencies
    install_dependencies
    
    echo "Local build environment setup complete!"
    return 0
}

# Setup for container build
setup_container_build() {
    local engine="$1"
    
    if [[ "$engine" == "docker" ]]; then
        echo "Setting up for Docker build..."
        
        # Check if Docker is available
        if ! check_docker; then
            echo "Docker not found."
            suggest_docker_install
            
            # Ask if user wants to install Docker now
            echo ""
            read -p "Would you like to install Docker now? [y/N] " install_docker
            if [[ "$install_docker" == "y" || "$install_docker" == "Y" ]]; then
                case "$DISTRO_FAMILY" in
                    debian|fedora|rhel|arch|suse|alpine)
                        echo "Please follow the installation instructions above."
                        echo "After installation, run this script again with the --docker flag."
                        ;;
                    macos)
                        echo "Please install Docker Desktop using the URL above."
                        echo "After installation, run this script again with the --docker flag."
                        ;;
                    *)
                        echo "Please install Docker following your distribution's instructions."
                        echo "After installation, run this script again with the --docker flag."
                        ;;
                esac
                return 1
            else
                # Check if Podman is available as a fallback
                if check_podman; then
                    echo "However, Podman is available as an alternative."
                    read -p "Would you like to use Podman instead? [Y/n] " use_podman
                    if [[ "$use_podman" != "n" && "$use_podman" != "N" ]]; then
                        CONTAINER_ENGINE="podman"
                        echo "Switching to Podman for container builds."
                        return setup_container_build "podman"
                    fi
                fi
                return 1
            fi
        fi
        
        # Check if user is in the docker group already
        if ! groups | grep -q docker; then
            echo "Your user is not in the 'docker' group yet."
            echo "This means you'll need to use 'sudo' with every Docker command or add yourself to the group."
            read -p "Would you like to add your user to the 'docker' group now? [Y/n] " add_to_group
            
            if [[ "$add_to_group" != "n" && "$add_to_group" != "N" ]]; then
                case "$DISTRO_FAMILY" in
                    alpine)
                        sudo addgroup "$(whoami)" docker
                        ;;
                    *)
                        sudo usermod -aG docker "$(whoami)"
                        ;;
                esac
                
                echo "User added to the 'docker' group."
                echo "To activate this change without logging out, run:"
                echo "  newgrp docker"
                
                read -p "Would you like to apply this change now with 'newgrp docker'? [Y/n] " run_newgrp
                if [[ "$run_newgrp" != "n" && "$run_newgrp" != "N" ]]; then
                    # We can't directly run newgrp in the script as it would start a new shell
                    # So we inform the user about what's happening
                    echo "After this script completes, please run 'newgrp docker' manually."
                    echo "This will let you use Docker commands without sudo."
                fi
            fi
        fi
        
        echo "Docker is available. You can build using Docker."
        return 0
        
    elif [[ "$engine" == "podman" ]]; then
        echo "Setting up for Podman build..."
        
        # Check if Podman is available
        if ! check_podman; then
            echo "Podman not found."
            suggest_podman_install
            
            # Ask if user wants to install Podman now
            echo ""
            read -p "Would you like to install Podman now? [y/N] " install_podman
            if [[ "$install_podman" == "y" || "$install_podman" == "Y" ]]; then
                case "$DISTRO_FAMILY" in
                    debian|fedora|rhel|arch|suse|alpine)
                        echo "Please follow the installation instructions above."
                        echo "After installation, run this script again with the --podman flag."
                        ;;
                    macos)
                        echo "Please install Podman using the commands above."
                        echo "After installation, run this script again with the --podman flag."
                        ;;
                    *)
                        echo "Please install Podman following your distribution's instructions."
                        echo "After installation, run this script again with the --podman flag."
                        ;;
                esac
                return 1
            else
                # Check if Docker is available as a fallback
                if check_docker; then
                    echo "However, Docker is available as an alternative."
                    read -p "Would you like to use Docker instead? [Y/n] " use_docker
                    if [[ "$use_docker" != "n" && "$use_docker" != "N" ]]; then
                        CONTAINER_ENGINE="docker"
                        echo "Switching to Docker for container builds."
                        return setup_container_build "docker"
                    fi
                fi
                return 1
            fi
        fi
        
        echo "Podman is available. You can build using Podman."
        return 0
        
    else
        echo "Unknown container engine: $engine"
        return 1
    fi
}

# Alias for Docker setup (for backward compatibility)
setup_docker_build() {
    setup_container_build "docker"
}

# Print instructions for local build
print_local_instructions() {
    # Only show local build instructions for Linux platforms
    if [[ "$OS_TYPE" != "linux" ]]; then
        echo ""
        echo "Local Build Instructions"
        echo "------------------------------------"
        echo "Direct SBCL builds are only supported on Linux."
        echo "Please use the container-based build method instead."
        echo ""
        return
    fi

    echo ""
    echo "Local Build Instructions"
    echo "------------------------------------"
    echo "To build OneFileLinux with SBCL, run:"
    echo ""
    echo "sbcl --load \"${PROJECT_ROOT}/${LISP_MAIN_PATH}\" -- [options]"
    echo ""
    echo "Common build options:"
    for profile in "${PROFILES[@]}"; do
        echo "  --profile=$profile"
    done
    echo "  --verbose            Enable verbose output"
    echo ""
    echo "Example:"
    echo "sbcl --load \"${PROJECT_ROOT}/${LISP_MAIN_PATH}\" -- --profile=${DEFAULT_PROFILE} --verbose"
    echo ""
}

# Print instructions for container build
print_container_instructions() {
    local engine="$1"
    
    echo ""
    echo "${engine^} Build Instructions"
    echo "------------------------------------"
    echo "To build using ${engine^} (recommended for clean, isolated builds):"
    echo ""
    echo "1. Build the container image:"
    echo "   ${engine} build -t onefilelinux-builder:latest -f ${PROJECT_ROOT}/${DOCKER_FILE_PATH} ."
    echo ""
    echo "2. Run the build inside the container:"
    echo "   ${engine} run --rm -v ${PROJECT_ROOT}/${OUTPUT_DIR}:/${OUTPUT_DIR} onefilelinux-builder:latest \\"
    echo "     --profile=${DEFAULT_PROFILE}"
    echo ""
    echo "The resulting EFI file will be available in the ${OUTPUT_DIR}/ directory"
    echo ""
    
    # Additional notes for non-Linux platforms
    if [[ "$OS_TYPE" != "linux" ]]; then
        echo "NOTE: For ${OS_TYPE^} users, container-based builds are the only supported method."
        if [[ "$OS_TYPE" == "macos" ]]; then
            echo "On macOS, ensure Docker Desktop is running or Podman machine is initialized."
        elif [[ "$OS_TYPE" == "windows" ]]; then
            echo "On Windows, ensure Docker Desktop is running or WSL2 with Podman is configured."
        fi
        echo ""
    fi
}

# For backward compatibility
print_docker_instructions() {
    print_container_instructions "docker"
}

# Main execution
main() {
    echo "=========================================="
    echo "  OneFileLinux Build Environment Setup"
    echo "=========================================="
    
    # Detect OS and distribution
    detect_distro
    
    # Process command line arguments
    process_args "$@"
    
    # Prompt user if mode not specified
    prompt_build_mode
    
    # For non-Linux platforms, adjust build mode if needed
    if [[ "$OS_TYPE" != "linux" ]]; then
        # Force container-based build for non-Linux
        if [[ "$BUILD_MODE" == "local" ]]; then
            echo "Direct local builds are not supported on ${OS_TYPE^}."
            echo "Switching to container-based build."
            BUILD_MODE="container"
        elif [[ "$BUILD_MODE" == "all" ]]; then
            echo "Direct local builds are not supported on ${OS_TYPE^}."
            echo "Using container-based build only."
            BUILD_MODE="container"
        fi
    fi
    
    # Setup based on selected mode
    case "$BUILD_MODE" in
        "local")
            if setup_local_build; then
                print_local_instructions
            fi
            ;;
        "docker")
            if setup_container_build "docker"; then
                print_container_instructions "docker"
            elif [[ "$OS_TYPE" == "linux" ]]; then
                # Only offer local setup as fallback on Linux
                echo "Docker setup failed. Would you like to try local setup instead? [y/N]"
                read -p "> " choice
                if [[ "$choice" == "y" || "$choice" == "Y" ]]; then
                    setup_local_build
                    print_local_instructions
                fi
            elif check_podman; then
                # On non-Linux, try Podman if Docker fails
                echo "Docker setup failed, but Podman is available."
                read -p "Would you like to use Podman instead? [Y/n] " use_podman
                if [[ "$use_podman" != "n" && "$use_podman" != "N" ]]; then
                    setup_container_build "podman"
                    print_container_instructions "podman"
                fi
            fi
            ;;
        "container")
            if setup_container_build "$CONTAINER_ENGINE"; then
                print_container_instructions "$CONTAINER_ENGINE"
            elif [[ "$OS_TYPE" == "linux" ]]; then
                # Only offer local setup as fallback on Linux
                echo "Container setup failed. Would you like to try local setup instead? [y/N]"
                read -p "> " choice
                if [[ "$choice" == "y" || "$choice" == "Y" ]]; then
                    setup_local_build
                    print_local_instructions
                fi
            elif [[ "$CONTAINER_ENGINE" == "docker" ]] && check_podman; then
                # On non-Linux, try alternative container engine
                echo "Docker setup failed, but Podman is available."
                read -p "Would you like to use Podman instead? [Y/n] " use_podman
                if [[ "$use_podman" != "n" && "$use_podman" != "N" ]]; then
                    setup_container_build "podman"
                    print_container_instructions "podman"
                fi
            elif [[ "$CONTAINER_ENGINE" == "podman" ]] && check_docker; then
                echo "Podman setup failed, but Docker is available."
                read -p "Would you like to use Docker instead? [Y/n] " use_docker
                if [[ "$use_docker" != "n" && "$use_docker" != "N" ]]; then
                    setup_container_build "docker"
                    print_container_instructions "docker"
                fi
            fi
            ;;
        "all")
            # For Linux, try both local and container builds
            if [[ "$OS_TYPE" == "linux" ]]; then
                setup_local_build
                echo ""
                
                # Try to set up the preferred container engine
                local container_setup_success=false
                if setup_container_build "$CONTAINER_ENGINE"; then
                    container_setup_success=true
                elif [[ "$CONTAINER_ENGINE" == "docker" ]] && check_podman; then
                    echo "Docker setup failed, but Podman is available."
                    read -p "Would you like to use Podman instead? [Y/n] " use_podman
                    if [[ "$use_podman" != "n" && "$use_podman" != "N" ]]; then
                        CONTAINER_ENGINE="podman"
                        if setup_container_build "podman"; then
                            container_setup_success=true
                        fi
                    fi
                elif [[ "$CONTAINER_ENGINE" == "podman" ]] && check_docker; then
                    echo "Podman setup failed, but Docker is available."
                    read -p "Would you like to use Docker instead? [Y/n] " use_docker
                    if [[ "$use_docker" != "n" && "$use_docker" != "N" ]]; then
                        CONTAINER_ENGINE="docker"
                        if setup_container_build "docker"; then
                            container_setup_success=true
                        fi
                    fi
                fi
                
                echo ""
                echo "===== Build Environment Setup Complete ====="
                print_local_instructions
                
                if $container_setup_success; then
                    print_container_instructions "$CONTAINER_ENGINE"
                fi
            else
                # For non-Linux, just do container setup
                if setup_container_build "$CONTAINER_ENGINE"; then
                    echo ""
                    echo "===== Build Environment Setup Complete ====="
                    print_container_instructions "$CONTAINER_ENGINE"
                fi
            fi
            ;;
    esac
    
    # Show appropriate help command based on OS and build mode
    if [[ "$OS_TYPE" == "linux" ]] && [[ "$BUILD_MODE" == "local" || "$BUILD_MODE" == "all" ]]; then
        echo "For more build options, run:"
        echo "sbcl --load \"${PROJECT_ROOT}/${LISP_MAIN_PATH}\" -- --help"
    fi
    echo ""
}

main "$@"