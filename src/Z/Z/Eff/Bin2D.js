
export const js_binEff_2d_new = () => ({
  _: {},
  size: 0,
  sizes: {},
});
export const js_binEff_2d_lookup =
  (Nothing) => (Just) => (k1) => (k2) => (st) => () => {
    const v = (st._[k1] || {})[k2];
    return v ? Just(v[0]) : Nothing;
  };
export const js_binEff_2d_insert =
  (unit) => (k1) => (k2) => (v) => (st) => () => {
    if (!st._[k1]) {
      st.size++;
    }
    st.sizes[k1] ||= 0;
    st._[k1] ||= {};
    if (!st._[k1][k2]) {
      st.sizes[k1]++;
    }
    st._[k1][k2] = [v];
    return unit;
  };
export const js_binEff_2d_delete = (unit) => (k1) => (k2) => (st) => () => {
  if (st._[k1]) {
    st.size--;
  }
  st.sizes[k1] ||= 0;
  if ((st._[k1] || {})[k2]) {
    st.sizes[k1]--;
  }
  delete (st._[k1] || {})[k2];
  return unit;
};
export const js_binEff_2d_sizeAt = (st) => (k) => () => {
  return st.sizes[k];
};
export const js_binEff_2d_valsAt = (st) => (k) => () => {
  return Object.values(st._[k]).map((ref) => ref[0]);
};
export const js_binEff_2d_all = (st) => () => {
  return Object.values(st._).flatMap((obj) =>
    Object.values(obj).map((ref) => ref[0]),
  );
};
export const js_binEff_2d_clearAt = (unit) => (k) => (st) => () => {
  if (st._[k]) {
    st.size--;
  }
  st._[k] = {};
  delete st.sizes[k];
  return unit;
};
export const js_binEff_2d_toForeignObjectAt = (k1) => (st) => () => {
  const res = {};
  for (const [k2, [v]] of Object.entries(st._[k1])) {
    res[k2] = v;
  }
  return res;
};
export const js_binEff_2d_addForeignObjectAt =
  (unit) => (k1) => (fo) => (st) => () => {
    if (!st._[k1]) {
      st.size++;
    }
    st._[k1] ||= {};
    for (const [k2, v] of Object.entries(fo)) {
      st._[k1][k2] = [v];
    }
    st.sizes[k1] = Object.keys(st._[k1]).length;
    return unit;
  };

export const js_binEff_2d_size = (st) => () => {
  return st.size;
};
export const js_binEff_2d_clear = (unit) => (st) => () => {
  st._ = {};
  st.size = 0;
  st.sizes = {};
  return unit;
};
