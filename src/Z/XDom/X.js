export const js2_domwNew = () => ({ currId: 1, elsById: { [1]: [] } });
export const js2_domwCurrEls = (domx) => () => {
  return domx.elsById[domx.currId];
};
export const js2_domwStart = (domx) => () => {
  const prevId = domx.currId;
  const currId = prevId + 1;
  domx.currId = currId;
  domx.elsById[currId] = [];
  return prevId;
};
export const js2_domwFinish = (prevId) => (domx) => () => {
  const els = js2_domwCurrEls(domx)();
  delete domx.elsById[domx.currId];
  domx.currId = prevId;
  return els;
};
export const js2_domwPush = (reactEl) => (domx) => () => {
  const els = domx.elsById[domx.currId];
  els.push(reactEl);
};

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
