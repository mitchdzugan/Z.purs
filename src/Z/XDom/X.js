export const js_domwNew = (fin) => () => ({ fin, curr: [] });
export const js_domwPause = (domx) => () => {
  if (domx.curr.length) {
    domx.fin.push(domx.curr);
  }
  const fin = domx.fin;
  domx.finx = [];
  domx.curr = [];
  return fin;
};
export const js_domwResume = (fin) => (domx) => () => {
  domx.fin = fin;
  domx.curr = [];
};
export const js_domwResults = (domx) => () => {
  const res = [];
  for (const els of domx.fin) {
    for (const el of els) {
      res.push(el);
    }
  }
  for (const el of domx.curr) {
    res.push(el);
  }
  return res;
};
export const js_domwPush = (el) => (domx) => () => {
  domx.curr.push(el);
};
export const js_domwConcat = (els) => (domx) => () => {
  for (const el of els) {
    domx.curr.push(el);
  }
};
export const js_attrwNew = () => ({
  classNames: [],
  children: [],
  styles: {},
  rawAttrs: {},
});
export const js_attrwResults = (attrx) => () => {
  const res = { attrs: attrx.rawAttrs, children: attrx.children };
  if (attrx.classNames.length) {
    res.attrs.className = attrx.classNames.join(" ");
  }
  if (Object.values(attrx.styles).length) {
    res.attrs.style = attrx.styles;
  }
  return res;
};
export const js_attrwResetClassnames = (attrx) => () => {
  attrx.classNames = [];
};
export const js_attrwResetChildren = (attrx) => () => {
  attrx.children = [];
};
export const js_attrwResetStyles = (attrx) => () => {
  attrx.styles = {};
};
export const js_attrwAddClassname = (cn) => (attrx) => () => {
  attrx.classNames.push(cn);
};
export const js_attrwAddChildren = (els) => (attrx) => () => {
  for (const el of els) {
    attrx.children.push(el);
  }
};
export const js_attrwAddStyle = (styleK) => (styleV) => (attrx) => () => {
  attrx.styles[styleK] = styleV;
};
export const js_attrwAddAttr = (attrK) => (attrV) => (attrx) => () => {
  attrx.rawAttrs[attrK] = attrV;
};
