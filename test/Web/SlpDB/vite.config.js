import { defineConfig } from "vite";
import tailwindcss from "@tailwindcss/vite";
import { fileURLToPath } from "url";
import path from "path";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

export default defineConfig({
  build: { watch: { buildDelay: 1000 } },
  plugins: [tailwindcss()],
  publicDir: path.join(__dirname, "public"),
});
