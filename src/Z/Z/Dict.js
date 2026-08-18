class Dict {
  constructor(d) {
    this._ = d;
  }
}

const maybe = (J, N, ref) => (ref ? J(ref[0]) : N);
export const js_empty = new Dict({});
export const js_insert = (k) => (v) => (d) => new Dict({ ...d._, [k]: [v] });
export const js_lookup = (J) => (N) => (k) => (d) => maybe(J, N, d._[k]);
export const js_encode = (EncV) => (d) =>
  Object.entries(d._).map(([k, v]) => ({ k, v: EncV(v) }));
export const js_fromEncodedDict = (encoded) => {
  const res = {};
  for (const { k, v } of encoded) {
    res[k] = v;
  }
  return new Dict(res);
};
