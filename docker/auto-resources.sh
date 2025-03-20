#!/bin/bash
# Script to automatically determine optimal resource allocation for Docker build

# Default values if detection fails
DEFAULT_MEM="4g"
DEFAULT_CPUS="2"

MIN_FREE_MEM_GB=4  # Minimum free memory to leave for the host system
MIN_FREE_CPUS=1    # Minimum free CPU cores to leave for the host system

# Get total system memory
if [[ "$OSTYPE" == "darwin"* ]]; then
    # macOS
    TOTAL_MEM_KB=$(sysctl -n hw.memsize | awk '{print $1/1024}')
    TOTAL_CPUS=$(sysctl -n hw.ncpu)
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # Linux
    TOTAL_MEM_KB=$(grep MemTotal /proc/meminfo | awk '{print $2}')
    TOTAL_CPUS=$(nproc)
else
    # Unsupported OS, use defaults
    echo "Unable to detect system resources on this OS. Using defaults."
    echo "DOCKER_MEMORY=$DEFAULT_MEM"
    echo "DOCKER_CPUS=$DEFAULT_CPUS"
    exit 0
fi

# Convert to GB and round down
TOTAL_MEM_GB=$((TOTAL_MEM_KB / 1024 / 1024))

# Calculate available resources, leaving some for the host
AVAIL_MEM_GB=$((TOTAL_MEM_GB - MIN_FREE_MEM_GB))
AVAIL_CPUS=$((TOTAL_CPUS - MIN_FREE_CPUS))

# Ensure minimums
if [ $AVAIL_MEM_GB -lt 2 ]; then
    AVAIL_MEM_GB=2
fi
if [ $AVAIL_CPUS -lt 1 ]; then
    AVAIL_CPUS=1
fi

# Use 75% of available memory for Docker if less than 10GB, otherwise 85%
if [ $AVAIL_MEM_GB -lt 10 ]; then
    DOCKER_MEM_GB=$((AVAIL_MEM_GB * 3 / 4))
else
    DOCKER_MEM_GB=$((AVAIL_MEM_GB * 85 / 100))
fi
DOCKER_MEM="${DOCKER_MEM_GB}g"

# Determine build flags
BUILD_FLAGS="--jobs=$AVAIL_CPUS"
if [ $DOCKER_MEM_GB -gt 16 ]; then
    # Plenty of memory, no need for special flags
    BUILD_FLAGS="$BUILD_FLAGS"
else
    # Limited memory, use memory optimization flags
    BUILD_FLAGS="$BUILD_FLAGS --use-swap"
fi

# Output results
echo "Detected system resources: ${TOTAL_MEM_GB}GB RAM, ${TOTAL_CPUS} CPU cores"
echo "Allocating: ${DOCKER_MEM} RAM, ${AVAIL_CPUS} CPU cores to Docker"

# Always set the variables for sourcing
DOCKER_MEMORY=$DOCKER_MEM
DOCKER_CPUS=$AVAIL_CPUS
BUILD_FLAGS=$BUILD_FLAGS

# Output for different modes
if [ "$1" == "--env" ]; then
    echo "DOCKER_MEMORY=$DOCKER_MEM"
    echo "DOCKER_CPUS=$AVAIL_CPUS"
    echo "BUILD_FLAGS=$BUILD_FLAGS"
    
    # Also export for sourcing
    export DOCKER_MEMORY
    export DOCKER_CPUS
    export BUILD_FLAGS
elif [ "$1" == "--export" ]; then
    echo "export DOCKER_MEMORY=$DOCKER_MEM"
    echo "export DOCKER_CPUS=$AVAIL_CPUS"
    echo "export BUILD_FLAGS=$BUILD_FLAGS"
else
    # Default output
    echo "DOCKER_MEMORY=$DOCKER_MEM"
    echo "DOCKER_CPUS=$AVAIL_CPUS"
    echo "BUILD_FLAGS=$BUILD_FLAGS"
fi