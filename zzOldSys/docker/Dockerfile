FROM --platform=$BUILDPLATFORM alpine:latest

# Set build arguments with defaults
ARG BUILDPLATFORM="linux/amd64"
ARG TARGETPLATFORM="linux/amd64"

# Print platform info for debugging
RUN echo "Building on $BUILDPLATFORM for $TARGETPLATFORM"

# Install build dependencies
RUN apk add --no-cache \
    bash \
    build-base \
    musl-dev \
    linux-headers \
    git \
    autoconf \
    automake \
    libtool \
    util-linux \
    util-linux-dev \
    xz-dev \
    zlib-dev \
    zstd-dev \
    lz4-dev \
    openssl-dev \
    libelf \
    elfutils-dev \
    bison \
    flex \
    ccache \
    upx \
    xz \
    zstd \
    curl \
    wget \
    sudo \
    gcc \
    g++ \
    make \
    patch \
    python3 \
    python3-dev \
    py3-setuptools \
    ncurses-dev \
    e2fsprogs \
    e2fsprogs-dev \
    coreutils \
    mtools \
    xorriso \
    squashfs-tools \
    kmod \
    bc \
    rsync \
    shadow \
    su-exec \
    # Full GNU utilities for kernel build (BusyBox lacks some required options)
    diffutils \
    findutils \
    # OpenSSL for kernel signing operations
    openssl \
    # Package config tool for build system
    pkgconf \
    # UUID libraries required for ZFS
    libuuid \
    # Extended attribute support required for ZFS
    attr-dev \
    # Device management support for ZFS
    eudev-dev \
    # RPC support required for ZFS
    libtirpc-dev \
    # Math libraries for GCC plugins
    gmp-dev \
    mpfr-dev \
    mpc1-dev \
    # Additional dependencies needed for ZFS
    libunwind-dev \
    libuuid \
    musl-dev \
    # Performance optimizations for archive extraction
    pigz \
    pv

# Create non-root user to run the build as
RUN adduser -D -u 1000 builder && \
    echo "builder ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/builder && \
    chmod 4755 /bin/busybox

# Set up build directories and cache locations
RUN mkdir -p /onefilelinux/build /onefilelinux/output /onefilelinux/.buildcache && \
    chown -R builder:builder /onefilelinux

# Create cache directories
RUN mkdir -p /onefilelinux/.buildcache/sources \
             /onefilelinux/.buildcache/packages \
             /onefilelinux/.buildcache/ccache \
             /onefilelinux/.buildcache/build && \
    chown -R builder:builder /onefilelinux/.buildcache

# Configure ccache for better performance
ENV CCACHE_DIR=/onefilelinux/.buildcache/ccache
ENV PATH=/usr/lib/ccache:$PATH
RUN su builder -c "ccache -M 5G" && \
    su builder -c "ccache -o compression=true" && \
    su builder -c "ccache -o compression_level=6" && \
    su builder -c "ccache -z"

# Copy entrypoint script
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

# Set workdir
WORKDIR /onefilelinux

# Install a healthcheck
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD ps aux | grep -v grep | grep -q "bash" || exit 1

# Set entrypoint
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["./build.sh"]