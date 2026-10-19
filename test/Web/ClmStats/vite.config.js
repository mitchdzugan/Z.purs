import { defineConfig } from "vite";
import tailwindcss from "@tailwindcss/vite";
import { visualizer } from "rollup-plugin-visualizer";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const publicDir = path.join(__dirname, "static");

const target = "http://localhost:4000";
const proxyServer = { proxy: { "/db": { target, changeOrigin: true } } };

export default defineConfig({
  plugins: [
    tailwindcss(),
    visualizer({
      open: false,
      filename: "stats.html",
      gzipSize: true,
      brotliSize: true,
    }),
  ],
  publicDir,
  server: proxyServer,
});
