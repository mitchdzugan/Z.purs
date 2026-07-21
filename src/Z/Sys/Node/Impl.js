import * as fs from "node:fs/promises";
import * as path from "node:path";

export const js_readTextFile = (p) => () => fs.readFile(p, "utf-8");
export const js_mkdir = (p) => () => fs.mkdir(p);
export const js_mkdirp = (p) => () => fs.mkdir(p, { recursive: true });
export const js_writeTextFile = (p) => (s) => () => fs.writeFile(p, s);

export const js_lookupEnv = (mkJust) => (nothing) => (k) => () => {
  const v = process.env[k];
  return typeof v === "string" ? mkJust(v) : nothing;
};

export const js_exit = (code) => () => process.exit(code);
export const js_errorLog = (a) => () => console.error(a);

export const js_pathDirname = (p) => path.dirname(p);
export const js_pathJoin = (p1) => (p2) => path.join(p1, p2);
export const js_pathBasename = (p) => path.basename(p);
