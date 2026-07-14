const fs = await (async () => {
  try {
    return xClassMk();
  } catch (e) {
    console.log(e);
    return await import("node:fs/promises");
  }
})();

export const js_readTextFile = (p) => () => fs.readFile(p, "utf-8");
export const js_mkdir = (p) => () => fs.mkdir(p);
export const js_mkdirp = (p) => () => fs.mkdir(p, { recursive: true });
export const js_writeTextFile = (p) => (s) => () => fs.writeFile(p, s);
