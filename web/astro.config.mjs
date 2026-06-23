import { defineConfig } from 'astro/config';

export default defineConfig({
  devToolbar: { enabled: false },
  outDir: '../dist',
  publicDir: 'public',
  build: {
    assets: '_assets',
  },
  vite: {
    optimizeDeps: {
      exclude: ['integra_wasm'],
    },
    ssr: {
      noExternal: ['integra_wasm'],
    },
    server: {
      watch: {
        ignored: ['../wasm/**'],
      },
    },
  },
});
