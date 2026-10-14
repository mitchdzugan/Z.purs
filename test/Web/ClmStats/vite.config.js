import { defineConfig } from "vite";
import tailwindcss from "@tailwindcss/vite";
import path from "path";

const pagesDir = process.env["CLM_STATS_PAGES_CO"];

if (!pagesDir) {
  throw "CLM_STATS_PAGES_CO required";
}

const publicDir = path.join(pagesDir, "docs");

export default defineConfig({
  build: { watch: { buildDelay: 100 } },
  plugins: [tailwindcss()],
  publicDir,
});
