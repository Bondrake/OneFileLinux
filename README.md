# OneRecovery

OneRecovery is a lightweight Linux distribution contained in a single EFI executable file that runs on any UEFI computer (PC or Mac) without installation. The system is deployed by copying a single file to the EFI system partition and configuring boot options.

<img width=600 alt="OneRecovery" src="https://hub.zhovner.com/img/one-file-linux.png" />

## Project History

OneRecovery is a fork of the original OneFileLinux project, which hasn't been maintained for over 5 years. This modernized version features:

- Updated Linux kernel (6.10.x) and Alpine Linux (3.21.0)
- ZFS filesystem support
- Enhanced system utilities for recovery and disk management
- Streamlined user experience with automatic root login
- Size and performance optimizations

## Key Advantages

- **Zero Installation Requirements**: No additional partitions needed; simply copy one file to the EFI system partition and add a boot entry to NVRAM.
  
- **No USB Flash Needed**: Once copied to the EFI partition, OneRecovery can boot any time from the system disk.
  
- **Direct Boot Capability**: Boots directly via UEFI firmware without requiring boot managers like GRUB or rEFInd.
  
- **Non-Disruptive Operation**: Can be configured for one-time boot, preserving the default boot sequence for subsequent reboots.
  
- **Encryption Compatibility**: Works alongside disk encryption solutions including macOS FileVault and Linux dm-crypt, as the EFI system partition remains unencrypted.

## Use Cases

OneRecovery provides direct hardware access capabilities not available in virtual machines, including:

- System recovery and data rescue operations
- Hardware diagnostics and testing
- Access to internal PCIe devices (such as WiFi cards for network diagnostics)
- Disk management and filesystem recovery with ZFS support

## Platform-Specific Installation

### macOS Installation

1. **Download OneRecovery.efi**
   From the releases page or build it yourself.

2. **Mount the EFI System Partition**
   ```bash
   diskutil list               # Identify your EFI partition (typically disk0s1)
   diskutil mount disk0s1      # Replace with your EFI partition identifier
   ```
   
   <img width="500" alt="macOS diskutil list EFI partition" src="https://hub.zhovner.com/img/diskutil-list-efi.png" />

3. **Copy OneRecovery.efi to the EFI Partition**
   ```bash
   cp ~/Downloads/OneRecovery.efi /Volumes/EFI/
   ```

4. **Configure Boot Options**
   
   Since macOS El Capitan, System Integrity Protection (SIP) requires boot option changes to be made from Recovery Mode:
   
   1. Check SIP status: `csrutil status` (Enabled by default)
   2. Restart and hold **CMD+R** during startup to enter Recovery Mode
   3. Open Terminal from Utilities menu
   4. Mount the EFI partition (step 2)
   5. Set the boot option:
      ```bash
      bless --mount /Volumes/EFI --setBoot --nextonly --file /Volumes/EFI/OneRecovery.efi
      ```
      
   This configures a one-time boot of OneRecovery, preserving your default boot order.

5. **Reboot to Start OneRecovery**
   
   After using OneRecovery, type `reboot` in the Linux console to return to macOS. Repeat steps 2 and 4 from Recovery Mode for subsequent uses.

### PC/Windows Installation

There are multiple methods to boot OneRecovery on PC systems. The following procedure works for most systems without built-in UEFI Shell access.

1. **Access the EFI System Partition**
   
   Windows 10+ systems installed in UEFI mode typically have a 100MB EFI partition.
   You will need either:
   - A Linux live USB to access this partition
   - An existing installation of OneRecovery via USB
   
2. **Configure NVRAM Boot Option**
   
   Using Linux, add a boot entry with efibootmgr:
   ```bash
   efibootmgr --disk /dev/sda --part 1 --create --label "OneRecovery" --loader /OneRecovery.efi
   ```
   
   Replace `/dev/sda` with your disk path and `--part 1` with your EFI partition number.

3. **Boot OneRecovery**
   
   Boot into your computer's boot menu (typically F12, F10, or Esc during startup) and select "OneRecovery".
   
   <img alt="Boot menu example" width="600" src="https://hub.zhovner.com/img/thinkpad-x220-boot-menu.png" />

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

2. **Install OneRecovery**
   
   Create the directory structure and copy the file:
   ```
   mkdir -p X:\EFI\BOOT          # Replace X: with your USB drive letter
   copy OneRecovery.efi X:\EFI\BOOT\BOOTx64.EFI
   ```

3. **Boot from USB**
   
   Select the USB drive from your computer's boot menu.



## Technical Overview

### Architecture
OneRecovery creates a single EFI executable file that contains a complete Linux system, allowing it to boot directly via UEFI without installation. The core components are:

- Linux kernel (6.10.14)
- Alpine Linux minimal rootfs (3.21.0)
- ZFS filesystem support (2.3.0-rc3)
- System utilities for recovery operations

### Key Features
- **Non-invasive**: Runs without modifying existing systems
- **Hardware access**: Direct access to hardware components (like PCIe WiFi cards)
- **Filesystem support**: Handles ext4, ZFS, FAT32, etc.
- **Disk management**: LVM, RAID, encryption (cryptsetup)
- **Network tools**: DHCP, SSH (dropbear), basic utilities
- **Recovery tools**: Disk utilities, filesystem tools, debootstrap

### Use Cases
- Recovering data from systems with inaccessible primary OS
- Working with hardware that can't be virtualized
- Penetration testing using internal hardware
- Emergency system recovery and maintenance
- Working alongside encrypted filesystems


## Building from Source

OneRecovery can be built from source using the FoxBuild system. The build environment is based on Alpine Linux and the vanilla Linux kernel.

### Prerequisites

- **Linux build environment** (required): The build process uses Linux-specific tools and chroot, which won't work directly on macOS
  - If using macOS, you'll need to use a Linux virtual machine or container
  - Docker or a Linux VM (via UTM/VirtualBox/Parallels) is recommended for macOS users
- Standard build tools (gcc, make, etc.)
- Approximately 5GB of free disk space
- Internet connection for downloading source components
- Required packages:
  - Linux: `wget`, `tar`, `xz-utils`, `flex`, `bison`, `libssl-dev` (or `openssl-devel` on Fedora/RHEL)
  - On macOS/Docker: Install wget if needed: `brew install wget` (only for downloading, actual build requires Linux)

### Build Process

The build system uses a sequence of numbered scripts with robust error handling:

1. `00_prepare.sh`: Detects OS, installs dependencies, and prepares the build environment
2. `01_get.sh`: Downloads and extracts Alpine Linux, Linux kernel, and ZFS sources
3. `02_chrootandinstall.sh`: Sets up the chroot environment and installs packages
4. `03_conf.sh`: Configures system services, network, auto-login, and kernel settings
5. `04_build.sh`: Builds the kernel, modules, ZFS support, and creates the EFI file
6. `99_cleanup.sh`: Removes build artifacts after successful build

All build scripts include:
- Comprehensive error handling with detailed diagnostic messages
- Prerequisite checking to ensure dependencies are met
- The ability to resume from checkpoints with the `--resume` flag
- Detailed logging to help troubleshoot build failures

### Build Instructions

1. Clone the repository:  
   ```bash
   git clone https://github.com/Bondrake/OneRecovery
   cd OneRecovery
   ```

2. Prepare the build environment:
   ```bash
   cd FoxBuild
   sudo ./00_prepare.sh     # Detects OS and installs required dependencies
   ```

3. Run the build scripts in sequence:  
   ```bash
   ./01_get.sh
   ./02_chrootandinstall.sh
   ./03_conf.sh
   ./04_build.sh
   ```
   
3. After successful completion, the `OneRecovery.efi` file will be created in the repository root directory.

4. Optional: Clean up build artifacts:
   ```bash
   ./99_cleanup.sh
   ```
