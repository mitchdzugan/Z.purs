export const js_binEff_new = () => ({ _: {}, size: 0, start: 0 });
export const js_binEff_lookup = (Nothing) => (Just) => (key) => (st) => () => {
  const v = st._[key];
  return v ? Just(v[0]) : Nothing;
};
export const js_binEff_insert = (unit) => (key) => (v) => (st) => () => {
  if (!st._[key]) {
    st.size++;
  }
  st._[key] = [v];
  return unit;
};
export const js_binEff_delete = (unit) => (key) => (st) => () => {
  if (st._[key]) {
    st.size--;
  }
  delete st._[key];
  return unit;
};
export const js_binEff_size = (st) => () => {
  return st.size;
};
export const js_binEff_vals = (st) => () => {
  return Object.values(st._).map((ref) => ref[0]);
};
export const js_binEff_clear = (unit) => (st) => () => {
  st._ = {};
  st.size = 0;
  st.start = 0;
  return unit;
};
export const js_binEff_toForeignObject = (st) => () => ({ ...st._ });
export const js_binEff_addForeignObject = (unit) => (fo) => (st) => () => {
  st._ = { ...st._, ...fo };
  st.size = Object.keys(st._).length;
  return unit;
};
