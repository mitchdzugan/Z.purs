import { defineConfig } from "vite";
import tailwindcss from "@tailwindcss/vite";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const publicDir = path.join(__dirname, "static");

const target = "http://localhost:4000";
const proxyServer = { proxy: { "/db": { target, changeOrigin: true } } };

export default defineConfig({
  build: { watch: { buildDelay: 100 } },
  plugins: [tailwindcss()],
  publicDir,
  server: proxyServer
});
