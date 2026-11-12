export const js_attrwNew = () => ({
  classNames: [],
  styles: {},
  rawAttrs: {},
});
export const js_attrwResults = (attrx) => () => {
  const res = attrx.rawAttrs;
  if (attrx.classNames.length) {
    res.className = attrx.classNames.join(" ");
  }
  if (Object.values(attrx.styles).length) {
    res.style = attrx.styles;
  }
  return res;
};
export const js_attrwResetClassnames = (attrx) => () => {
  attrx.classNames = [];
};
export const js_attrwResetStyles = (attrx) => () => {
  attrx.styles = {};
};
export const js_attrwAddClassname = (cn) => (attrx) => () => {
  attrx.classNames.push(cn);
};
export const js_attrwAddStyle = (styleK) => (styleV) => (attrx) => () => {
  attrx.styles[styleK] = styleV;
};
export const js_attrwAddAttr = (attrK) => (attrV) => (attrx) => () => {
  attrx.rawAttrs[attrK] = attrV;
};
