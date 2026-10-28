import { snapshot, go } from "fuzzysort";

export const js_snapshot = (els) => (keys) => snapshot(els, { keys });
export const js_goRaw = (els) => (keys) => (s) => (opts) =>
  go(s, els, { keys, ...opts });
export const js_goSnapshot = (snap) => (s) => (opts) => go(s, snap, opts);
export const js_encodeArrayOpts = (arrayOpts) => {
  const res = {};
  for (const [k, vs] of Object.entries(arrayOpts)) {
    if (vs.length > 0) {
      res[k] = vs[0];
    }
  }
  return res;
};
