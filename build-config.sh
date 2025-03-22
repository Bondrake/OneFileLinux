#!/bin/bash
# OneFileLinux build configuration 
# Contains all configurable parameters for the build bootstrapper

# URLs
QUICKLISP_URL="https://beta.quicklisp.org/quicklisp.lisp"
DOCKER_GPG_URL="https://download.docker.com/linux/DISTRO_ID/gpg"
DOCKER_REPO_URL="https://download.docker.com/linux/DISTRO_ID"

# Common Lisp libraries required
CL_LIBS=("uiop" "cl-ppcre" "alexandria")

# Package names by distribution family
# Debian-based systems
DEBIAN_PACKAGES=("sbcl" "curl" "build-essential")

# Red Hat-based systems using dnf
REDHAT_DNF_PACKAGES=("sbcl" "curl")

# Red Hat-based systems using yum
REDHAT_YUM_PACKAGES=("sbcl" "curl")

# Arch-based systems
ARCH_PACKAGES=("sbcl" "curl" "base-devel")

# SUSE-based systems
SUSE_PACKAGES=("sbcl" "curl")

# Alpine Linux
ALPINE_PACKAGES=("sbcl" "curl" "gcc" "musl-dev")

# Gentoo Linux
GENTOO_PACKAGES=("dev-lisp/sbcl" "net-misc/curl")

# Void Linux
VOID_PACKAGES=("sbcl" "curl" "base-devel")

# macOS (Homebrew)
MACOS_PACKAGES=("sbcl" "curl")

# Podman installation instructions by distribution
# Debian/Ubuntu
DEBIAN_PODMAN_INSTALL=(
  "sudo apt-get update"
  "sudo apt-get install -y podman podman-docker"
)

# Fedora (has Podman by default)
FEDORA_PODMAN_INSTALL=(
  "sudo dnf install -y podman podman-docker"
)

# RHEL/CentOS
RHEL_PODMAN_INSTALL=(
  "sudo yum install -y podman podman-docker"
)

# Arch
ARCH_PODMAN_INSTALL=(
  "sudo pacman -Sy podman podman-docker"
)

# SUSE
SUSE_PODMAN_INSTALL=(
  "sudo zypper install -y podman podman-docker"
)

# Alpine
ALPINE_PODMAN_INSTALL=(
  "sudo apk add --no-cache podman podman-docker"
)

# Docker installation instructions by distribution
# Debian/Ubuntu
DEBIAN_DOCKER_INSTALL=(
  "sudo apt-get update"
  "sudo apt-get install -y ca-certificates curl gnupg"
  "sudo install -m 0755 -d /etc/apt/keyrings"
  "curl -fsSL https://download.docker.com/linux/DISTRO_ID/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg"
  "sudo chmod a+r /etc/apt/keyrings/docker.gpg"
  "echo \"deb [arch=\$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/DISTRO_ID \$(lsb_release -cs) stable\" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null"
  "sudo apt-get update"
  "sudo apt-get install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin"
  "sudo usermod -aG docker \"\$(whoami)\""
  "echo \"NOTE: Run 'newgrp docker' or log out and back in to apply group changes\""
)

# Fedora
FEDORA_DOCKER_INSTALL=(
  "sudo dnf -y install dnf-plugins-core"
  "sudo dnf config-manager --add-repo https://download.docker.com/linux/fedora/docker-ce.repo"
  "sudo dnf install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin"
  "sudo systemctl start docker"
  "sudo systemctl enable docker"
  "sudo usermod -aG docker \"\$(whoami)\""
  "echo \"NOTE: Run 'newgrp docker' or log out and back in to apply group changes\""
)

# RHEL/CentOS
RHEL_DOCKER_INSTALL=(
  "sudo yum install -y yum-utils"
  "sudo yum-config-manager --add-repo https://download.docker.com/linux/centos/docker-ce.repo"
  "sudo yum install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin"
  "sudo systemctl start docker"
  "sudo systemctl enable docker"
  "sudo usermod -aG docker \"\$(whoami)\""
  "echo \"NOTE: Run 'newgrp docker' or log out and back in to apply group changes\""
)

# Arch
ARCH_DOCKER_INSTALL=(
  "sudo pacman -Sy docker"
  "sudo systemctl start docker.service"
  "sudo systemctl enable docker.service"
  "sudo usermod -aG docker \"\$(whoami)\""
  "echo \"NOTE: Run 'newgrp docker' or log out and back in to apply group changes\""
)

# SUSE
SUSE_DOCKER_INSTALL=(
  "sudo zypper install -y docker"
  "sudo systemctl start docker"
  "sudo systemctl enable docker"
  "sudo usermod -aG docker \"\$(whoami)\""
  "echo \"NOTE: Run 'newgrp docker' or log out and back in to apply group changes\""
)

# Alpine
ALPINE_DOCKER_INSTALL=(
  "sudo apk add --no-cache docker"
  "sudo rc-update add docker boot"
  "sudo service docker start"
  "sudo addgroup \$(whoami) docker"
  "echo \"NOTE: Run 'newgrp docker' or log out and back in to apply group changes\""
)

# Docker Desktop URL
DOCKER_DESKTOP_URL="https://www.docker.com/products/docker-desktop/"

# Docker docs URL
DOCKER_DOCS_URL="https://docs.docker.com/engine/install/"

# Podman docs URL
PODMAN_DOCS_URL="https://podman.io/getting-started/installation"

# Container engine options
CONTAINER_ENGINES=("docker" "podman")
DEFAULT_CONTAINER_ENGINE="docker"

# Path configuration 
LISP_MAIN_PATH="main.lisp"
LISP_ASDF_PATH="onefilelinux.asd"
DOCKER_FILE_PATH="docker/Dockerfile"
OUTPUT_DIR="output"

# OS detection strings
OS_TYPES=("linux" "macos" "windows" "unknown")
OS_FAMILIES=("linux" "darwin" "windows" "unknown")

# Windows detection strings
WINDOWS_PATTERNS=("CYGWIN*" "MINGW*" "MSYS*")

# File paths for detection
DEBIAN_OS_RELEASE="/etc/os-release"
DEBIAN_VERSION_FILE="/etc/debian_version"
REDHAT_RELEASE_FILE="/etc/redhat-release"
ALPINE_RELEASE_FILE="/etc/alpine-release"
LSB_RELEASE_FILE="/etc/lsb-release"

# Build profiles
PROFILES=("minimal" "standard" "full")
DEFAULT_PROFILE="minimal"

# OS-specific messages
MACOS_LIMITED_MSG="NOTE: macOS is supported for container-based builds only"
WINDOWS_LIMITED_MSG="NOTE: Windows is supported for container-based builds only"
UNKNOWN_OS_MSG="Warning: Unknown operating system. Container-based builds are recommended for non-Linux platforms"

# Distribution mapping
# Maps distribution IDs to families for easier package selection
declare -A DISTRO_FAMILY_MAP
DISTRO_FAMILY_MAP=(
  ["ubuntu"]="debian"
  ["debian"]="debian"
  ["pop"]="debian"
  ["mint"]="debian"
  ["elementary"]="debian"
  ["zorin"]="debian"
  ["kali"]="debian"
  ["parrot"]="debian"
  
  ["fedora"]="fedora"
  
  ["rhel"]="rhel"
  ["centos"]="rhel"
  ["rocky"]="rhel"
  ["alma"]="rhel"
  ["ol"]="rhel"
  ["scientific"]="rhel"
  ["amzn"]="rhel"
  
  ["arch"]="arch"
  ["manjaro"]="arch"
  ["endeavouros"]="arch"
  ["garuda"]="arch"
  
  ["opensuse"]="suse"
  ["suse"]="suse"
  ["sles"]="suse"
  
  ["alpine"]="alpine"
  ["gentoo"]="gentoo"
  ["void"]="void"
  ["macos"]="macos"
)