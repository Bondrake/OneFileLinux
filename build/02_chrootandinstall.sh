#!/bin/bash
#
# Configure Alpine Linux chroot and install packages
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

# Initialize script with standard header (prints banner)
initialize_script

# Start timing for this script
start_timing "02_chrootandinstall: Setup"

# Configure DNS for the chroot environment
log "INFO" "Setting up DNS for chroot environment"
cat > alpine-minirootfs/etc/resolv.conf << EOF
nameserver 1.1.1.1
nameserver 8.8.4.4
EOF

# Create installation script
log "INFO" "Creating installation script"
# Determine which packages to install based on module configuration
# Define base packages without line continuations to avoid parsing issues
PACKAGES="openrc nano mc bash parted dropbear dropbear-ssh efibootmgr e2fsprogs e2fsprogs-extra dosfstools dmraid fuse gawk grep sed util-linux wget"

# Add ZFS support if enabled
if [ "${INCLUDE_ZFS:-true}" = "true" ]; then
    # Add ZFS package - we'll handle failures during installation
    log "INFO" "Including ZFS runtime support using Alpine packages"
    PACKAGES="$PACKAGES zfs"
fi

# Add BTRFS support if enabled
if [ "${INCLUDE_BTRFS:-false}" = "true" ]; then
    PACKAGES="$PACKAGES btrfs-progs"
    log "INFO" "Including Btrfs support"
fi

# Add recovery tools if enabled
if [ "${INCLUDE_RECOVERY_TOOLS:-true}" = "true" ]; then
    PACKAGES="$PACKAGES testdisk ddrescue rsync unzip tar"
    log "INFO" "Including recovery tools"
fi

# Add network tools if enabled
if [ "${INCLUDE_NETWORK_TOOLS:-true}" = "true" ]; then
    # Note: We use dropbear-ssh instead of openssh-client to avoid conflicts
    # Added nftables as the modern replacement for iptables
    PACKAGES="$PACKAGES curl rsync iperf3 tcpdump nftables"
    log "INFO" "Including network tools with nftables (modern firewall)"
fi

# Add crypto support if enabled
if [ "${INCLUDE_CRYPTO:-true}" = "true" ]; then
    PACKAGES="$PACKAGES cryptsetup lvm2 mdadm"
    log "INFO" "Including encryption support"
fi

# Add TUI dependencies if enabled
if [ "${INCLUDE_TUI:-true}" = "true" ]; then
    PACKAGES="$PACKAGES ncurses-terminfo-base less"
    log "INFO" "Including TUI dependencies"
fi

# Add advanced filesystem tools if enabled
if [ "${INCLUDE_ADVANCED_FS:-false}" = "true" ]; then
    # Note: Using gptfdisk instead of gdisk (gdisk is provided by gptfdisk package)
    ADVANCED_FS_PACKAGES="ntfs-3g xfsprogs gptfdisk exfatprogs f2fs-tools"
    PACKAGES="$PACKAGES $ADVANCED_FS_PACKAGES"
    log "INFO" "Including advanced filesystem tools: $ADVANCED_FS_PACKAGES"
fi

# Add disk and hardware diagnostics if enabled
if [ "${INCLUDE_DISK_DIAG:-false}" = "true" ]; then
    DISK_DIAG_PACKAGES="smartmontools hdparm nvme-cli dmidecode lshw"
    PACKAGES="$PACKAGES $DISK_DIAG_PACKAGES"
    log "INFO" "Including disk and hardware diagnostics: $DISK_DIAG_PACKAGES"
fi

# Add network diagnostics if enabled
if [ "${INCLUDE_NETWORK_DIAG:-false}" = "true" ]; then
    NETWORK_DIAG_PACKAGES="ethtool nmap wireguard-tools openvpn"
    PACKAGES="$PACKAGES $NETWORK_DIAG_PACKAGES"
    log "INFO" "Including network diagnostics and VPN tools: $NETWORK_DIAG_PACKAGES"
fi

# Add system tools if enabled
if [ "${INCLUDE_SYSTEM_TOOLS:-false}" = "true" ]; then
    SYSTEM_TOOLS_PACKAGES="htop strace pciutils usbutils"
    PACKAGES="$PACKAGES $SYSTEM_TOOLS_PACKAGES"
    log "INFO" "Including advanced system tools: $SYSTEM_TOOLS_PACKAGES"
fi

# Add data recovery tools if enabled (photorec is part of testdisk)
if [ "${INCLUDE_DATA_RECOVERY:-false}" = "true" ]; then
    DATA_RECOVERY_PACKAGES="testdisk"
    PACKAGES="$PACKAGES $DATA_RECOVERY_PACKAGES"
    log "INFO" "Including advanced data recovery tools: $DATA_RECOVERY_PACKAGES"
fi

# Add boot repair tools if enabled
if [ "${INCLUDE_BOOT_REPAIR:-false}" = "true" ]; then
    BOOT_REPAIR_PACKAGES="grub"
    PACKAGES="$PACKAGES $BOOT_REPAIR_PACKAGES"
    log "INFO" "Including boot repair tools: $BOOT_REPAIR_PACKAGES"
fi

# Add advanced editors if enabled
if [ "${INCLUDE_EDITORS:-false}" = "true" ]; then
    EDITORS_PACKAGES="vim tmux jq"
    PACKAGES="$PACKAGES $EDITORS_PACKAGES"
    log "INFO" "Including advanced text editors: $EDITORS_PACKAGES"
fi

# Add security tools if enabled
if [ "${INCLUDE_SECURITY:-false}" = "true" ]; then
    SECURITY_PACKAGES="openssl"
    PACKAGES="$PACKAGES $SECURITY_PACKAGES"
    log "INFO" "Including security tools: $SECURITY_PACKAGES"
fi

# Add extra packages if specified
if [ -n "${EXTRA_PACKAGES:-}" ]; then
    # Convert comma-separated list to space-separated list
    EXTRA_PACKAGES_LIST=$(echo "$EXTRA_PACKAGES" | tr ',' ' ')
    PACKAGES="$PACKAGES $EXTRA_PACKAGES_LIST"
    log "INFO" "Including extra packages: $EXTRA_PACKAGES_LIST"
fi

end_timing

# Start timing for package installation
start_timing "02_chrootandinstall: Package installation"

cat > alpine-minirootfs/mk.sh << EOF
#!/bin/ash
set -e

echo "[INFO] Setting hostname"
echo onefilelinux > /etc/hostname && hostname -F /etc/hostname
echo 127.0.1.1 onefilelinux onefilelinux >> /etc/hosts

echo "[INFO] Setting up APK cache directory"
mkdir -p /var/cache/apk
touch /var/cache/apk/APKCACHE.init

echo "[INFO] Updating package lists"
apk update --cache-dir=/var/cache/apk

# Start fresh with repositories to avoid duplicates
echo "[INFO] Setting up clean repository configuration"

# Create a clean repositories file
cat > /etc/apk/repositories << EOF
http://dl-cdn.alpinelinux.org/alpine/v3.21/main
http://dl-cdn.alpinelinux.org/alpine/v3.21/community
http://dl-cdn.alpinelinux.org/alpine/edge/testing
EOF

apk update --cache-dir=/var/cache/apk

echo "[INFO] Upgrading installed packages"
echo "[DEBUG] Running: apk upgrade --cache-dir=/var/cache/apk"
# Run upgrade without ignore flag to detect errors
apk upgrade --cache-dir=/var/cache/apk

echo "[INFO] Installing required packages"
echo "[DEBUG] Package list: $PACKAGES"

# Install packages with proper error detection
echo "[DEBUG] Running: apk add --cache-dir=/var/cache/apk --no-interactive $PACKAGES"
apk add --cache-dir=/var/cache/apk --no-interactive $PACKAGES

echo "[INFO] Verifying core packages installation"
# Check status of core packages that are essential
CORE_PACKAGES="bash openrc util-linux e2fsprogs efibootmgr"
for pkg in $CORE_PACKAGES; do
    if ! apk info --cache-dir=/var/cache/apk -e "$pkg" >/dev/null 2>&1; then
        echo "[ERROR] Essential package $pkg not installed"
    fi
done

# Check ZFS package installation
if [ "${INCLUDE_ZFS:-true}" = "true" ]; then
    echo "[INFO] Verifying ZFS package installation"
    # Verify ZFS package was properly installed
    if ! apk info --cache-dir=/var/cache/apk -e "zfs" >/dev/null 2>&1; then
        # Report error but don't attempt recovery
        echo "[ERROR] ZFS package not installed. Check repository configuration."
    fi
fi

echo "[INFO] Cleaning package cache"
rm /var/cache/apk/*

echo "[INFO] Installation completed successfully"
exit 0
EOF

# Make installation script executable
chmod +x alpine-minirootfs/mk.sh
log "INFO" "Installation script created and made executable"

# Execute chroot using the environment-aware helper
log "INFO" "Entering chroot environment to install packages"

# Use our cross-environment chroot helper
end_timing

# Start timing for chroot execution
start_timing "02_chrootandinstall: Chroot execution"

if type prepare_chroot >/dev/null 2>&1 && type cleanup_chroot >/dev/null 2>&1; then
    # Prepare the chroot environment
    prepare_chroot "alpine-minirootfs"
    
    # Execute the chroot command
    log "INFO" "Running installation script..."
    chroot alpine-minirootfs /bin/ash /mk.sh
    chroot_status=$?
    
    # Clean up the chroot environment
    cleanup_chroot "alpine-minirootfs"
    
    if [ $chroot_status -ne 0 ]; then
        log "ERROR" "Chroot installation failed with status: $chroot_status"
        exit $chroot_status
    fi
else
    # Fall back to simple chroot if the helpers aren't available
    log "WARNING" "Chroot helpers not available, using basic chroot"
    chroot alpine-minirootfs /bin/ash /mk.sh
fi

log "SUCCESS" "Chroot installation completed successfully"

# Clean up installation script
rm alpine-minirootfs/mk.sh
log "INFO" "Removed installation script"

# End timing for chroot execution
end_timing

# Print final status
print_script_end

# If this is the final script being run, finalize the timing log
if [ "${FINALIZE_TIMING_LOG:-false}" = "true" ]; then
    finalize_timing_log
fi

# Add an exit trap to debug how the script is exiting
trap 'echo "DEBUG: 02_chrootandinstall.sh exiting with code $? (normal exit)"; exit 0' EXIT