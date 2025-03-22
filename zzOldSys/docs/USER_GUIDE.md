# OneFileLinux User Guide

This comprehensive guide explains how to build, install, and use OneFileLinux for system recovery operations.

## Table of Contents

1. [Overview](#overview)
2. [Building OneFileLinux](#building-onefilelinux)
   - [Using Docker](#using-docker-recommended)
   - [Building Natively](#building-natively)
   - [Build Options](#build-options)
3. [Installation](#installation)
   - [macOS Installation](#macos-installation)
   - [PC/Windows Installation](#pcwindows-installation)
   - [USB Flash Drive Installation](#usb-flash-drive-installation)
4. [Using OneFileLinux](#using-onefilelinux)
   - [Text User Interface](#text-user-interface)
   - [Common Recovery Operations](#common-recovery-operations)
   - [Working with Filesystems](#working-with-filesystems)
   - [Network Connectivity](#network-connectivity)
5. [Troubleshooting](#troubleshooting)
   - [Boot Issues](#boot-issues)
   - [Hardware Compatibility](#hardware-compatibility)
   - [Common Errors](#common-errors)
6. [Advanced Usage](#advanced-usage)
   - [Custom Configurations](#custom-configurations)
   - [Recovery Scenarios](#recovery-scenarios)
   - [Performance Considerations](#performance-considerations)
7. [Developer Documentation](#developer-documentation)
   - [Build System Architecture](#build-system-architecture)
   - [Feature Flag System](#feature-flag-system)
   - [Kernel Configuration System](#kernel-configuration-system)
   - [Build Environments](#build-environments)
   - [CI/CD System](#cicd-system)

## Overview

OneFileLinux is a lightweight Linux distribution contained in a single EFI executable file that runs on any UEFI computer (PC or Mac) without installation. It provides a comprehensive set of tools for system recovery, data rescue, and hardware diagnostics.

### Key Features

- **Single EFI File**: Boots directly from UEFI without additional bootloaders
- **Advanced Filesystems**: Support for ZFS, Btrfs, ext4, XFS, and more
- **Hardware Diagnostics**: Tools for hardware testing and analysis
- **Network Support**: Ethernet, WiFi, and remote recovery capabilities
- **Data Recovery**: Specialized tools for rescuing data from failed systems
- **Boot Repair**: Tools to fix common boot problems across operating systems
- **Text UI**: Full-featured text-based user interface for easy navigation
- **Size Optimized**: Minimal builds under 50MB, full builds under 150MB

### Project Revival

OneFileLinux has been revived and completely redesigned from the ground up, transforming a previously unmaintained project into a robust, modern recovery solution. Key improvements include:

- **Modular Architecture**: Complete rebuild with a layered, maintainable design
- **Error Resilience**: Comprehensive error handling and recovery mechanisms
- **Modern Foundation**: Updated Linux kernel (6.12) and Alpine Linux (3.21)
- **Advanced Storage Support**: Full ZFS and Btrfs integration
- **Adaptive Building**: Flexible build system with multiple configuration options
- **Streamlined Experience**: Automatic root login and intuitive text-based interface
- **Size Optimization**: Ultra-compact builds as small as 4MB for minimal configurations
- **Continuous Integration**: Automated build and test pipeline with GitHub Actions
- **Docker Integration**: Containerized build environment for consistent results

## Building OneFileLinux

### Using Docker (Recommended)

The easiest way to build OneFileLinux is using Docker, which provides a consistent build environment regardless of your host system.

#### Prerequisites

- Docker installed and running
- Git
- At least 4GB free RAM
- At least 10GB free disk space

#### Build Steps

1. Clone the repository:
   ```bash
   git clone https://github.com/zhovner/OneFileLinux.git
   cd OneFileLinux/docker
   ```

2. Make the build script executable:
   ```bash
   chmod +x build-onefilelinux.sh
   ```

3. Run the build with default settings:
   ```bash
   ./build-onefilelinux.sh
   ```

4. For a full build with all features:
   ```bash
   ./build-onefilelinux.sh -b "--full"
   ```

5. For a minimal build:
   ```bash
   ./build-onefilelinux.sh -b "--minimal"
   ```

#### Docker Build Options

The `build-onefilelinux.sh` script supports several options:

```
Usage: ./build-onefilelinux.sh [options]

Options:
  -h, --help            Display this help message
  -c, --clean           Clean the Docker environment before building
  -v, --verbose         Enable verbose output
  -b, --build-args ARG  Pass build arguments to the build script
  -e, --env-file FILE   Specify a custom .env file
  -i, --interactive     Run in interactive mode (shell inside container)
  -p, --pull            Pull the latest base image before building
  --no-cache            Build the Docker image without using cache
```

#### Build Artifacts

After a successful build, the output file (`OneFileLinux.efi`) will be placed in the `output/` directory in the root of the repository.

### Building Natively

If you prefer to build on your local system without Docker:

#### Prerequisites

For Debian/Ubuntu-based systems:
```bash
sudo apt-get update
sudo apt-get install build-essential git autoconf automake libtool \
  util-linux libelf-dev libssl-dev zlib1g-dev libzstd-dev liblz4-dev \
  upx xz-utils zstd curl wget sudo python3 gcc g++ make patch \
  libncurses-dev e2fsprogs coreutils mtools xorriso squashfs-tools
```

For Alpine Linux:
```bash
apk add build-base git autoconf automake libtool util-linux elfutils-dev \
  openssl-dev zlib-dev zstd-dev lz4-dev upx xz zstd curl wget sudo \
  python3 gcc g++ make patch ncurses-dev e2fsprogs coreutils mtools \
  xorriso squashfs-tools
```

#### Build Steps

1. Clone the repository:
   ```bash
   git clone https://github.com/zhovner/OneFileLinux.git
   cd OneFileLinux
   ```

2. Run the build scripts sequentially:
   ```bash
   cd build
   ./build.sh
   ```

3. Or run each build step manually:
   ```bash
   ./00_prepare.sh   # Prepare the build environment
   ./01_get.sh       # Download and extract sources
   ./02_chrootandinstall.sh  # Set up chroot and install packages
   ./03_conf.sh      # Configure the system
   ./04_build.sh     # Build the final EFI file
   ```

4. After successful completion, the `OneFileLinux.efi` file will be created in the repository root directory.

5. Optional: Clean up build artifacts:
   ```bash
   ./99_cleanup.sh
   ```

### Build Options

OneFileLinux offers several build configurations to balance features and size:

#### Build Types

| Build Type | Description | Size | Command |
|------------|-------------|------|---------|
| Minimal | Core functionality only | ~30-50MB | `--minimal` |
| Standard | Basic recovery features | ~60-90MB | (default) |
| Full | All features and tools | ~100-150MB | `--full` |

**Standard Build Capabilities:**
- **Performance**: Balanced multi-core support with hardware crypto acceleration
- **Hardware Support**: Extensive coverage for storage controllers (SATA, AHCI, NVMe, USB), input devices, and basic framebuffer graphics
- **Filesystems**: Complete support for ZFS, Ext4, and other common filesystems (BTRFS optional)
- **Networking**: Full TCP/IP stack with routing, firewall capabilities, and diagnostics
- **Security**: Comprehensive cryptography with LUKS disk encryption and LVM support
- **System Footprint**: Moderately sized with broad hardware compatibility for effective recovery operations

#### Advanced Package Groups

You can customize your build with these package groups:

| Package Group | Size Impact | Description | Flag | Included Packages |
|---------------|-------------|-------------|------|-------------------|
| Advanced FS | ~10MB | Extra filesystem tools | `--with-advanced-fs` | ntfs-3g, xfsprogs, gptfdisk, exfatprogs, f2fs-tools |
| Disk Diagnostics | ~15MB | Hardware testing tools | `--with-disk-diag` | smartmontools, hdparm, nvme-cli, dmidecode, lshw |
| Network Diagnostics | ~12MB | Network diagnostics | `--with-network-diag` | ethtool, nmap, wireguard-tools, openvpn |
| System Tools | ~8MB | Advanced system utilities | `--with-system-tools` | htop, strace, pciutils, usbutils |
| Data Recovery | ~20MB | Data rescue utilities | `--with-data-recovery` | testdisk (includes photorec) |
| Boot Repair | ~15MB | Bootloader repair tools | `--with-boot-repair` | grub |
| Advanced Editors | ~5MB | Text editors and tools | `--with-editors` | vim, tmux, jq |
| Security Tools | ~10MB | Security analysis tools | `--with-security` | openssl |

#### Detailed Build Script Options

The `build.sh` script accepts numerous options to customize your build. Here's a comprehensive list of all available options:

##### Build Types
```
--minimal              Minimal build optimized for size (~30-50% smaller)
--full                 Full build with all available components
```

##### Size Optimization Options
```
--with-compression     Enable EFI file compression (default: yes)
--without-compression  Disable EFI file compression (faster boot)
--compression-tool=TOOL Select compression tool (upx, xz, zstd) (default: upx)
```

##### Build Performance Options
```
--use-cache            Enable source and build caching (default: yes)
--no-cache             Disable source and build caching
--cache-dir=DIR        Set cache directory (default: ~/.onefilelinux/cache)
--jobs=N               Set number of parallel build jobs (default: CPU cores)
--keep-ccache          Keep compiler cache between builds (default: yes)
--no-keep-ccache       Clear compiler cache between builds
--use-swap             Create swap file if memory is low (default: no)
--no-swap              Do not create swap file even if memory is low
--interactive-config   Use interactive kernel configuration (menuconfig)
--no-interactive-config Use non-interactive kernel config (default)
```

##### Security Options
```
--password=PASS        Set custom root password (CAUTION: visible in process list)
--random-password      Generate random root password (default)
--no-password          Create root account with no password (unsafe)
--password-length=N    Set length of random password (default: 12)
```

##### Optional Modules
```
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
```

##### Advanced Package Groups
```
--with-advanced-fs     Include advanced filesystem tools
--without-advanced-fs  Exclude advanced filesystem tools
--with-disk-diag       Include disk and hardware diagnostics
--without-disk-diag    Exclude disk and hardware diagnostics
--with-network-diag    Include network diagnostics and VPN tools
--without-network-diag Exclude network diagnostics and VPN tools
--with-system-tools    Include advanced system utilities
--without-system-tools Exclude advanced system utilities
--with-data-recovery   Include data recovery utilities
--without-data-recovery Exclude data recovery utilities
--with-boot-repair     Include boot repair tools
--without-boot-repair  Exclude boot repair tools
--with-editors         Include advanced text editors
--without-editors      Exclude advanced text editors
--with-security        Include security tools
--without-security     Exclude security tools
--with-all-advanced    Include all advanced package groups
--without-all-advanced Exclude all advanced package groups
```

##### Configuration Management
```
--save-config          Save current configuration as default
--show-config          Display current build configuration
```

#### Common Build Command Examples

```bash
# Minimal build with only ZFS support
./build.sh --minimal --with-zfs --without-all-advanced

# Standard build with advanced filesystem tools and data recovery
./build.sh --with-advanced-fs --with-data-recovery

# Full featured build with everything included
./build.sh --full

# Custom build with specific tools
./build.sh --with-zfs --with-btrfs --with-network-tools --with-disk-diag

# Minimal build with high compression
./build.sh --minimal --compression-tool=xz

# Build without compression for faster boot time
./build.sh --without-compression

# Custom build with specific performance settings
./build.sh --jobs=8 --cache-dir=/tmp/cache --use-swap

# Build with interactive kernel configuration
./build.sh --interactive-config

# Build with a custom root password
./build.sh --password=mypassword

# Build with a specific random password length
./build.sh --random-password --password-length=16
```

#### EFI Partition Size Considerations

When planning to use OneFileLinux, keep in mind these EFI partition size guidelines:

- **Minimal build**: Even small 100MB EFI partitions are more than sufficient
- **Standard build**: Standard 100MB EFI partition is recommended
- **Full build**: Standard 100MB EFI partition is typically sufficient

Most modern systems have EFI partitions of 100MB or larger, which is adequate for any OneFileLinux build.

## Installation

### MacOS Installation

1. **Download OneFileLinux.efi**
   From the releases page or build it yourself.

2. **Mount the EFI System Partition**
   ```bash
   diskutil list               # Identify your EFI partition (typically disk0s1)
   diskutil mount disk0s1      # Replace with your EFI partition identifier
   ```

3. **Copy OneFileLinux.efi to the EFI Partition**
   ```bash
   cp ~/Downloads/OneFileLinux.efi /Volumes/EFI/
   ```

4. **Configure Boot Options**
   
   Since macOS El Capitan, System Integrity Protection (SIP) requires boot option changes to be made from Recovery Mode:
   
   1. Check SIP status: `csrutil status` (Enabled by default)
   2. Restart and hold **CMD+R** during startup to enter Recovery Mode
   3. Open Terminal from Utilities menu
   4. Mount the EFI partition (step 2)
   5. Set the boot option:
      ```bash
      bless --mount /Volumes/EFI --setBoot --nextonly --file /Volumes/EFI/OneFileLinux.efi
      ```
      
   This configures a one-time boot of OneFileLinux, preserving your default boot order.

5. **Reboot to Start OneFileLinux**
   
   After using OneFileLinux, type `reboot` in the Linux console to return to macOS. Repeat steps 2 and 4 from Recovery Mode for subsequent uses.

### PC (Windows/Linux) Installation

There are multiple methods to boot OneFileLinux on PC systems. The following procedure works for most systems without built-in UEFI Shell access.

1. **Access the EFI System Partition**
   
   Windows 10+ systems installed in UEFI mode typically have a 100MB EFI partition.
   You will need either:
   - A Linux live USB to access this partition
   - An existing installation of OneFileLinux via USB
   
2. **Configure NVRAM Boot Option**
   
   Using Linux, add a boot entry with efibootmgr:
   ```bash
   efibootmgr --disk /dev/sda --part 1 --create --label "OneFileLinux" --loader /OneFileLinux.efi
   ```
   
   Replace `/dev/sda` with your disk path and `--part 1` with your EFI partition number.

3. **Boot OneFileLinux**
   
   Boot into your computer's boot menu (typically F12, F10, or Esc during startup) and select "OneFileLinux".

### USB Flash Drive Installation

For portable use or when direct EFI partition access is difficult:

1. **Format a USB Drive with GPT Partition Scheme**
   
   In Windows:
   ```
   1. Open Administrator Command Prompt
   2. Run diskpart
   3. list disk
   4. select disk N (replace N with your USB drive number)
   5. clean
   6. convert gpt
   7. create partition primary
   8. format fs=fat32 quick
   9. assign
   10. exit
   ```
   
   In macOS:
   ```bash
   diskutil list                        # Find your USB drive
   diskutil eraseDisk FAT32 ONEFILELINUX GPT /dev/diskN  # Replace diskN with your USB
   ```
   
   In Linux:
   ```bash
   sudo gdisk /dev/sdX                  # Replace sdX with your USB drive
   # Create a new GPT table (o), create a partition (n), write changes (w)
   sudo mkfs.vfat -F 32 /dev/sdX1       # Format the partition
   ```

2. **Install OneFileLinux**
   
   Create the directory structure and copy the file:
   
   Windows:
   ```
   mkdir -p X:\EFI\BOOT          # Replace X: with your USB drive letter
   copy OneFileLinux.efi X:\EFI\BOOT\BOOTx64.EFI
   ```
   
   macOS/Linux:
   ```bash
   mkdir -p /Volumes/ONEFILELINUX/EFI/BOOT   # macOS
   # OR
   mkdir -p /mnt/usb/EFI/BOOT               # Linux (mount point may vary)
   
   cp OneFileLinux.efi /Volumes/ONEFILELINUX/EFI/BOOT/BOOTx64.EFI   # macOS
   # OR
   cp OneFileLinux.efi /mnt/usb/EFI/BOOT/BOOTx64.EFI               # Linux
   ```

3. **Boot from USB**
   
   Select the USB drive from your computer's boot menu.

## Using OneFileLinux

### Text User Interface

When you boot OneFileLinux, it automatically logs in as root and launches the text-based user interface (TUI). The TUI provides an easy-to-use menu system for common recovery operations.

#### Login Information

- **Username**: root
- **Password**: 
  - For GitHub Actions release builds: The default password is `onefilelinux`
  - For local builds: The password is randomly generated during build and saved in `onefilelinux-password.txt` in the build directory
  - If you specified a custom password during build with `--password=PASS`, use that password

#### Main Menu

The main menu provides access to the following functions:

1. **System Information**: Hardware details, disk information, and system status
2. **Filesystem Tools**: Mount, unmount, check, and repair filesystems
3. **Data Recovery**: Tools for rescuing lost or damaged data
4. **Disk Management**: Partition, format, and manage disks
5. **Network Tools**: Configure network interfaces and run diagnostics
6. **Boot Repair**: Fix boot issues on Windows, Linux, and macOS
7. **Advanced Tools**: Additional system utilities and diagnostic tools
8. **Help**: Documentation and usage information

Navigate the menu using arrow keys, Tab, and Enter. Press 'q' or Escape to go back or exit menus.

### Common Recovery Operations

#### Mounting Filesystems

1. From the main menu, select "Filesystem Tools"
2. Choose "Mount Filesystem"
3. Select the partition to mount
4. Specify the mount point (or use the default)
5. Select filesystem type (if not automatically detected)

Example command-line alternative:
```bash
mount /dev/sda1 /mnt
```

#### Checking and Repairing Filesystems

1. From the main menu, select "Filesystem Tools"
2. Choose "Check/Repair Filesystem"
3. Select the partition to check
4. Choose between "Check Only" or "Check and Repair"

Example command-line alternatives:
```bash
# For ext2/3/4
fsck.ext4 -f /dev/sda1

# For ZFS
zpool import -f poolname
zpool scrub poolname

# For Btrfs
btrfs check /dev/sda1
```

#### Recovering Deleted Files

1. From the main menu, select "Data Recovery"
2. Choose "Recover Deleted Files"
3. Select the partition to scan
4. Select file types to recover
5. Specify a destination for recovered files

Example command-line alternative:
```bash
testdisk /dev/sda
```

#### Disk Partitioning

1. From the main menu, select "Disk Management"
2. Choose "Partition Disk"
3. Select the disk to partition
4. Follow the interactive prompts

Example command-line alternatives:
```bash
fdisk /dev/sda
# OR
gdisk /dev/sda   # For GPT partitioning
```

#### Fixing Boot Issues

1. From the main menu, select "Boot Repair"
2. Choose the operating system type (Windows, Linux, macOS)
3. Follow the guided repair process

### Working with Filesystems

#### ZFS Operations

OneFileLinux includes comprehensive ZFS support. Common operations:

```bash
# Import pool
zpool import -f poolname

# Check pool status
zpool status poolname

# Repair pool (scrub)
zpool scrub poolname

# Export pool cleanly
zpool export poolname

# Mount ZFS dataset
zfs mount poolname/dataset

# List all datasets
zfs list

# Take a snapshot
zfs snapshot poolname/dataset@snapshot1
```

#### Btrfs Operations

If your build includes Btrfs support:

```bash
# Mount Btrfs filesystem
mount -t btrfs /dev/sda1 /mnt

# Check filesystem
btrfs check /dev/sda1

# Repair filesystem
btrfs check --repair /dev/sda1

# List subvolumes
btrfs subvolume list /mnt

# Mount specific subvolume
mount -t btrfs -o subvol=subvolname /dev/sda1 /mnt
```

#### Working with Encrypted Volumes

```bash
# Open LUKS encrypted volume
cryptsetup luksOpen /dev/sda1 mydisk

# Mount the decrypted volume
mount /dev/mapper/mydisk /mnt

# Close encrypted volume
umount /mnt
cryptsetup luksClose mydisk
```

### Network Connectivity

#### Configuring Wired Network

1. From the main menu, select "Network Tools"
2. Choose "Configure Network Interface"
3. Select the Ethernet interface
4. Choose between DHCP or Static IP configuration

Example command-line alternative:
```bash
# Using DHCP
dhclient eth0

# Static IP configuration
ip addr add 192.168.1.100/24 dev eth0
ip route add default via 192.168.1.1
echo "nameserver 8.8.8.8" > /etc/resolv.conf
```

#### Configuring Wireless Network

If your build includes wireless tools:

1. From the main menu, select "Network Tools"
2. Choose "Configure Wireless Network"
3. Select the wireless interface
4. Scan for networks and select one
5. Enter the network password

Example command-line alternative:
```bash
# Scan for networks
iwlist wlan0 scan

# Connect to WPA/WPA2 network
wpa_passphrase MyNetwork MyPassword > /etc/wpa_supplicant.conf
wpa_supplicant -B -i wlan0 -c /etc/wpa_supplicant.conf
dhclient wlan0
```

#### Remote Access

If your build includes SSH:

1. From the main menu, select "Network Tools"
2. Choose "Enable SSH Server"
3. Set a root password when prompted

The system will display the IP address and connection instructions.

Example command-line alternative:
```bash
# Start SSH server
passwd root  # Set a password
/etc/init.d/sshd start

# Check your IP address
ip addr show
```

## Troubleshooting

### Boot Issues

#### System Doesn't Boot OneFileLinux

1. Verify that your system boots in UEFI mode, not legacy BIOS
2. Check that the EFI file is in the correct location
3. For USB boot, ensure the drive is formatted as GPT with FAT32
4. Try using the UEFI boot menu (F12, F10, Esc, etc. during startup)

#### Black Screen After Booting

1. Try booting with basic video mode (add `nomodeset` to kernel parameters)
2. If using NVIDIA graphics, try the `nouveau.modeset=0` kernel parameter

#### No Keyboard or Mouse Input

1. Check that USB ports are working
2. Try different USB ports, particularly USB 2.0 ports
3. For exotic hardware, try a PS/2 keyboard if available

### Hardware Compatibility

#### Storage Devices Not Detected

1. Check SATA/NVMe controller mode in BIOS (AHCI mode is preferred)
2. For NVMe drives, ensure your build includes NVMe support
3. For unusual storage controllers, load additional drivers:
   ```bash
   modprobe driver_name
   ```

#### Network Interfaces Not Working

1. Check if the interface is detected:
   ```bash
   ip link show
   ```
2. For wireless adapters, verify driver loading:
   ```bash
   lspci -k | grep -A 3 Network
   ```
3. Load additional drivers if needed:
   ```bash
   modprobe driver_name
   ```

### Common Errors

#### "No such file or directory" when mounting

The specified device doesn't exist or has a different name. Check available devices:
```bash
fdisk -l
# OR
lsblk
```

#### "Unknown filesystem type" when mounting

Your build may not include support for that filesystem. Try specifying the filesystem type:
```bash
mount -t filesystem_type /dev/device /mount_point
```

#### "Can't read superblock" when checking filesystem

The filesystem may be severely corrupted. Try alternative superblocks:
```bash
# For ext filesystems, find backup superblocks
mke2fs -n /dev/device

# Then use a backup superblock
fsck.ext4 -b 32768 /dev/device
```

#### ZFS pool can't be imported

Try forcing the import:
```bash
zpool import -f pool_name
# If that fails, try with all pools
zpool import -fA
# For severe cases, try 
zpool import -fFX pool_name
```

## Advanced Usage

### Custom Configurations

#### Adding Custom Utilities

If you've built OneFileLinux yourself, you can add custom utilities by:

1. Modifying the package list in `02_chrootandinstall.sh`
2. Adding custom scripts to the `zfiles/` directory
3. Rebuilding the system

#### Creating Custom Recovery Scripts

Create custom recovery scripts by adding them to the system:

1. Mount your root filesystem:
   ```bash
   mount /dev/sda1 /mnt
   ```

2. Create a script in a persistent location:
   ```bash
   vi /mnt/usr/local/bin/my-recovery.sh
   chmod +x /mnt/usr/local/bin/my-recovery.sh
   ```

### Recovery Scenarios

#### Recovering from Deleted Partition Table

1. Boot into OneFileLinux
2. Use TestDisk to scan for lost partitions:
   ```bash
   testdisk /dev/sda
   ```
3. Follow the prompts to search for lost partitions
4. Write the recovered partition table

#### Rescuing Files from Failed Drive

1. Boot into OneFileLinux
2. If the drive is physically failing, create a disk image:
   ```bash
   ddrescue /dev/failed_drive /mnt/backup/disk.img /mnt/backup/logfile
   ```
3. Mount the image and recover files:
   ```bash
   mount -o loop /mnt/backup/disk.img /mnt/recovered
   ```

#### Fixing Boot Problems

1. Boot into OneFileLinux
2. For Linux boot issues:
   ```bash
   mount /dev/sda1 /mnt            # Mount root filesystem
   mount /dev/sda2 /mnt/boot       # Mount boot filesystem if separate
   mount --bind /dev /mnt/dev
   mount --bind /proc /mnt/proc
   mount --bind /sys /mnt/sys
   chroot /mnt
   grub-install /dev/sda
   update-grub
   exit
   ```

3. For Windows boot issues, use the Boot Repair menu option

### Performance Considerations

#### Working with Large Drives

For large drives (>2TB), use these optimizations:

1. Set a larger block size when creating filesystems:
   ```bash
   mkfs.ext4 -b 4096 /dev/device
   ```

2. For large file operations, adjust buffer size:
   ```bash
   cp --buffer-size=16M source destination
   ```

#### Memory Optimization

If OneFileLinux is running slowly due to memory constraints:

1. Create and use a swap file:
   ```bash
   dd if=/dev/zero of=/tmp/swap bs=1M count=1024
   mkswap /tmp/swap
   swapon /tmp/swap
   ```

2. Clear disk caches if needed:
   ```bash
   echo 3 > /proc/sys/vm/drop_caches
   ```

#### Speeding Up Filesystem Checks

For large filesystems, speed up checks by:

1. Using multiple passes with optimized options:
   ```bash
   # For ext4
   fsck.ext4 -C0 -f -y /dev/device
   ```

2. For ZFS scrubs, set a higher priority:
   ```bash
   zpool scrub -p high poolname
   ```

---

## Developer Documentation

This section provides technical documentation for developers who want to understand or contribute to the OneFileLinux build system.

### Build System Architecture

OneFileLinux uses a modular build system designed to create a small, single-file EFI executable containing a complete Linux environment:

#### Library Scripts (80-89 range)

- **80_common.sh**: Core utilities, logging, and environment detection
- **81_error_handling.sh**: Error handling and recovery mechanisms
- **82_build_helper.sh**: File operations, environment adaptations, and feature flag parsing
- **83_config_helper.sh**: Configuration management
- **84_build_core.sh**: Kernel building, ZFS module building, and EFI file creation

#### Build Scripts (00-10, 99 range)

Sequential build steps:
- **01_get.sh**: Download Alpine Linux, Linux kernel, and ZFS sources
- **02_chrootandinstall.sh**: Configure Alpine Linux chroot and install packages
- **03_conf.sh**: Configure system services and apply kernel configuration overlays
- **04_build.sh**: Build Linux kernel and create EFI file

#### Master Build Scripts

- **build.sh**: Unified build script (local build, assumes Alpine environment)
- **docker/build-onefilelinux.sh**: Docker-based build launcher (recommended for cross-platform)

### Feature Flag System

OneFileLinux's feature flags control which components are included in the build, affecting size and functionality:

#### High-Level Build Profiles

| Profile | Flag | Description | Typical Size |
|---------|------|-------------|--------------|
| Minimal | `--minimal` | Essential functionality only | ~4MB |
| Standard | (default) | Balanced configuration | ~20MB |
| Full | `--full` | All features included | ~40-60MB |

##### Understanding Build Configurations

###### Minimal Build

The minimal build creates an ultra-small EFI file with only essential functionality, optimized for emergency scenarios where space is extremely limited.

What's Included:
- Base system (openrc, bash, parted)
- Basic EFI boot functionality
- Essential filesystem support (ext4, FAT/vFAT)
- Command-line interface

What's Excluded:
- ZFS and other advanced filesystem support
- Recovery tools (testdisk, ddrescue)
- Network tools and diagnostics
- Encryption support
- Text User Interface (TUI)
- Hardware diagnostics
- All advanced package groups

Impact on Usage:
- Command-line only interface (no TUI)
- Limited recovery capabilities
- Basic filesystem operations only
- Reduced hardware support
- Minimal network functionality

Best Uses:
- Emergency boot scenarios
- Systems with very small EFI partitions
- Creating a minimal recovery option
- Base for custom minimal builds
- Ultra-fast boot requirements

###### Standard Build

The standard build provides a balanced configuration suitable for most recovery operations while maintaining reasonable size.

What's Included:
- Complete multi-core processor support
- Hardware acceleration (Intel AES-NI, Padlock)
- Comprehensive storage drivers (SATA, AHCI, NVMe, USB)
- Full ZFS support with all compression options
- Common filesystems (Ext4, vFAT, XFS)
- Complete TCP/IP networking stack
- Firewall capabilities (Netfilter/NFTables)
- Disk encryption (LUKS/dm-crypt)
- Logical Volume Management (LVM)
- Text-based user interface

Impact on System:
- Moderate memory usage
- Balanced boot time
- Good hardware compatibility
- Comprehensive recovery capabilities
- Full filesystem support for data recovery
- Network diagnostics for remote recovery

Best Uses:
- Standard recovery operations
- Data rescue from most filesystem types
- System repair on typical hardware
- Network-based recovery scenarios
- Boot repair operations

#### Core Feature Flags

##### Core Package Groups

These package groups are enabled by default in standard builds and can be individually controlled:

| Package Group | Enabled By | Included Packages |
|---------------|------------|-------------------|
| Base System | (always included) | openrc, nano, mc, bash, parted, dropbear, dropbear-ssh, efibootmgr, e2fsprogs, e2fsprogs-extra, dosfstools, dmraid, fuse, gawk, grep, sed, util-linux, wget |
| ZFS Support | `INCLUDE_ZFS` | zfs, util-linux-dev, util-linux-misc, util-linux, util-linux-bash-completion |
| BTRFS Support | `INCLUDE_BTRFS` | btrfs-progs |
| Recovery Tools | `INCLUDE_RECOVERY_TOOLS` | testdisk, ddrescue, rsync, unzip, tar |
| Network Tools | `INCLUDE_NETWORK_TOOLS` | curl, rsync, iperf3, tcpdump, nftables |
| Crypto Support | `INCLUDE_CRYPTO` | cryptsetup, lvm2, mdadm |
| Text UI | `INCLUDE_TUI` | ncurses-terminfo-base, less |

| Flag | Default | Description |
|------|---------|-------------|
| `--with-zfs` / `--without-zfs` | ON | ZFS filesystem support |
| `--with-network-tools` / `--without-network-tools` | ON | Network support |
| `--with-crypto` / `--without-crypto` | ON | Encryption support |
| `--with-tui` / `--without-tui` | ON | Text user interface |
| `--minimal-kernel` / `--standard-kernel` | standard | Kernel size optimization |

#### Advanced Package Groups

| Flag | Default | Description |
|------|---------|-------------|
| `--with-advanced-fs` / `--without-advanced-fs` | OFF | Advanced filesystem tools |
| `--with-disk-diag` / `--without-disk-diag` | OFF | Hardware diagnostics |
| `--with-data-recovery` / `--without-data-recovery` | OFF | Data recovery utilities |
| `--with-boot-repair` / `--without-boot-repair` | OFF | Boot repair tools |

All features are controlled through the `parse_build_flags()` function in `82_build_helper.sh`, ensuring consistent behavior across build environments.

### Kernel Configuration System

The kernel configuration system balances size with functionality:

1. Base configurations:
   - **minimal.config**: Size-optimized minimal kernel
   - **standard.config**: Balanced configuration with common drivers

2. Feature overlays: Conditionally applied based on feature flags
   - **zfs-support.conf**: ZFS filesystem kernel support
   - **network-tools.conf**: Network protocol and driver support
   - **crypto-support.conf**: Encryption support

The overlay system in `03_conf.sh` respects feature flags, particularly `INCLUDE_MINIMAL_KERNEL`, which skips most overlays for minimal builds.

### Build Environments

OneFileLinux supports multiple build environments, with Docker being the recommended approach for cross-platform development:

#### Docker Build (Recommended)

```bash
cd docker
./build-onefilelinux.sh -b "--minimal"  # or any other build flags
```

The Docker approach:
- Works on any system that supports Docker (Linux, macOS, Windows)
- Handles all dependencies automatically
- Produces consistent results across different host systems
- Properly manages permissions and resource allocation

#### Local Build

```bash
cd build
./build.sh --minimal  # or any other build flags
```

Local builds:
- Assume an Alpine Linux-based environment
- Require all dependencies to be installed
- Need appropriate permissions for chroot operations

### CI/CD System

GitHub Actions workflow in `.github/workflows/docker-build.yml`:
- Builds multiple configurations in parallel (minimal, standard, full)
- Uses the same Docker container as local builds
- Creates artifacts for each build configuration
- Creates release packages for tagged builds that match 'v*' pattern
- Uses a default root password of 'onefilelinux' for all GitHub Actions release builds, unlike local builds which generate random passwords by default

### Best Practices for Development

1. **Use Docker for Development**:
   - Provides the most consistent build environment
   - Works across all platforms
   - Eliminates dependency and permission issues

2. **Maintain Small Image Size**:
   - OneFileLinux's primary advantage is being a tiny, single-file EFI executable (minimal: ~4MB, standard: ~20MB)
   - Make features optional with feature flags
   - Consider size impact for all changes

3. **Follow the Library Pattern**:
   - Use established library functions
   - Maintain separation of concerns
   - Respect feature flags throughout the codebase

4. **Test Across Environments**:
   - Verify builds work in Docker and GitHub Actions
   - Test both minimal and full configurations
   - Boot test on real hardware

This User Guide is continuously improved. Please refer to the official documentation repository for the latest version and additional information.