import { prepare, go } from "fuzzysort";

class ResProxy {
  #trueRes;
  constructor(trueRes) {
    this.#trueRes = trueRes;
  }
  get obj() {
    return this.#trueRes.obj.el;
  }
  get score() {
    return this.#trueRes.score;
  }
  get target() {
    return this.#trueRes.target;
  }
  get indexes() {
    return this.#trueRes.indexes;
  }
}

const mkResProxy = (res) => new ResProxy(res);

export const js_snapshot = (elsIn) => (keyFns) => {
  const keys = Array.from({ length: keyFns.length }, (_, ind) => ind);
  const els = elsIn.map((el) => {
    const res = { el };
    keyFns.forEach((keyFn, ind) => {
      res[ind] = prepare(keyFn(el));
    });
    return res;
  });
  return { keys, els };
};
export const js_goRaw = (els) => (keys) => (s) => (opts) =>
  go(s, els, { keys, ...opts });
export const js_goSnapshot =
  ({ keys, els }) =>
  (s) =>
  (opts) => {
    return go(s, els, { keys, ...opts }).map(mkResProxy);
  };
export const js_encodeArrayOpts = (arrayOpts) => {
  const res = {};
  for (const [k, vs] of Object.entries(arrayOpts)) {
    if (vs.length > 0) {
      res[k] = vs[0];
    }
  }
  return res;
};
