class Ref {
  constructor(v) {
    this._ = v;
  }
}
export const js_ref_new = (v) => () => new Ref(v);
export const js_ref_get = (ref) => () => ref._;
export const js_ref_set = (unit) => (v) => (ref) => () => {
  ref._ = v;
  return unit;
};
