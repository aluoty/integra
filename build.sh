#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

# Install Rust if not present
if ! command -v cargo &>/dev/null; then
  echo "Installing Rust..."
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
  source "$HOME/.cargo/env"
fi

export PATH="$HOME/.cargo/bin:$PATH"

# Add WASM target (skip if using NixOS system rust which handles this differently)
if command -v rustup &>/dev/null; then
  rustup target add wasm32-unknown-unknown --toolchain stable
fi

# Install wasm-bindgen-cli if not present
if ! command -v wasm-bindgen &>/dev/null; then
  echo "Installing wasm-bindgen-cli..."
  cargo install wasm-bindgen-cli --locked
fi

CARGO_CFG=()
# NixOS needs a custom wasm-ld wrapper
if [ -d /nix/store ]; then
  BOOTSTRAP_LD="/nix/store/ig3dxi9sbg0jnkid4s673mnz4kkbfwa4-rustc-bootstrap-1.95.0/lib/rustlib/x86_64-unknown-linux-gnu/bin/rust-lld"
  if [ -f "$BOOTSTRAP_LD" ]; then
    ln -sf "$BOOTSTRAP_LD" /tmp/wasm-ld
  fi
  if [ -f /tmp/wasm-ld ]; then
    CARGO_CFG=(--config 'target.wasm32-unknown-unknown.linker="/tmp/wasm-ld"')
  fi
fi

echo "Building WASM crate..."
cargo build --release --target wasm32-unknown-unknown "${CARGO_CFG[@]}" --manifest-path wasm/Cargo.toml

echo "Generating JS bindings..."
mkdir -p web/src/wasm
wasm-bindgen \
  --target web \
  --out-dir web/src/wasm \
  wasm/target/wasm32-unknown-unknown/release/integra_wasm.wasm

echo "Building Astro site..."
cd web
npm install
npm run build:site

echo "Done. Output in $(realpath ../dist/)"
