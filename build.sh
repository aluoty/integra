#!/usr/bin/env bash
set -euo pipefail

export PATH="$HOME/.cargo/bin:$PATH"
cd "$(dirname "$0")"

CARGO_CFG=()
if [ -f /nix/store/yhmi70ln28n1j6wn82h61b8r8q4g562i-rustc-1.95.0/bin/rustc ]; then
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
