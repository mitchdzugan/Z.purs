export const js_effArray_new = (init) => () => ({ init, arr: [...init] });
export const js_effArray_getPartial = ({ arr }) => arr;
export const js_effArray_freeze = (effArray) => () => [
  ...js_effArray_getPartial(effArray),
];
export const js_effArray_reset = (effArray) => () => {
  effArray.arr = [...effArray.init];
};
export const js_effArray_clear = (effArray) => () => {
  effArray.arr = [];
};
export const js_effArray_cons = (v) => (effArray) => () => {
  effArray.arr.push(v);
};
