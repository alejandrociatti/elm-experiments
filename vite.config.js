import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';

export default defineConfig({
  root: 'src',
  publicDir: 'src/assets',
  plugins: [elmPlugin()],
  build: {
    outDir: '../dist',
    emptyOutDir: true,
  }
});