#!/bin/bash
set -e

PROJECT_ROOT=$(pwd)

echo "Building Rust library..."
cd Core
cargo build --release
cd ..

echo "Generating pkg-config file..."
cat > imago.pc << EOF
prefix=${PROJECT_ROOT}
exec_prefix=\${prefix}
libdir=\${prefix}/Core/target/release
includedir=\${prefix}/Core/include

Name: imago
Description: Industrial grade image processing library
Version: 0.1.0
Libs: -L\${libdir} -limago
Cflags: -I\${includedir}
EOF

echo "Installing pkg-config file..."
mkdir -p ~/.local/lib/pkgconfig
cp imago.pc ~/.local/lib/pkgconfig/

echo "Adding to PKG_CONFIG_PATH..."
export PKG_CONFIG_PATH="$HOME/.local/lib/pkgconfig:$PKG_CONFIG_PATH"

echo "Verifying pkg-config setup..."
pkg-config --exists imago && echo "✓ pkg-config found imago" || echo "✗ pkg-config setup failed"