#!/usr/bin/env bash
set -euo pipefail

npm install -g wasm-pack
cd wasm
wasm-pack build --target web --release
cp -r pkg ../web/src/wasm
cd ../web
npm install
npm run build
