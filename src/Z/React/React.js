import { createElement, Fragment, useState, StrictMode } from "react";

export const js_helloReact = createElement(
  "div",
  {},
  "<xDom>Hello React!</xDom>",
);

const StateComponent = ({ renderEls, initialState, pure }) => {
  const [state, setState] = useState(initialState);
  return createElement(
    Fragment,
    {},
    ...renderEls(state)((st) => pure(setState(st))),
  );
};

const KeyComponent = ({ children }) => {
  return children;
};

export const js_textEl = (s) => s;
export const js_renderFragment = (children) =>
  createElement(Fragment, {}, ...children);
export const js_childrenNull = null;
export const js_childrenSingle = (child) => child;
export const js_childrenArray = (children) => children;
export const js_renderEl = (el) => (props) => (children) =>
  createElement(el, props, ...children);
export const js_propsFromPropWs = (getK) => (getV) => (kvs) => {
  const res = {};
  for (const kv of kvs) {
    res[getK(kv)] = getV(kv);
  }
  return res;
};
export const js_withState = (pure) => (renderEls) => (initialState) =>
  createElement(StateComponent, { renderEls, initialState, pure });

export const js_strict = (el) => createElement(StrictMode, {}, el);
