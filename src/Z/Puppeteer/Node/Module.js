import puppeteer from "puppeteer";
import assert from "node:assert";

function assertExists(v) {
  assert.notEqual(v, null);
  assert.notEqual(v, undefined);
  return v;
}

export const js_launchPuppeteer = (opts) => () => puppeteer.launch(opts);
export const js_browserClose = (b) => () => b.close();
export const js_newPage = (b) => () => b.newPage();
export const js_setViewport = (width) => (height) => (page) => () =>
  page.setViewport({ width, height });
export const js_goto = (url) => (opts) => (page) => () => {
  return page.goto(url, opts);
};
export const js_waitForSelector = (sel) => (opts) => (page) => () => {
  return page.waitForSelector(sel, opts);
};
export const js_PageOrElement_P = (p) => p;
export const js_PageOrElement_E = (e) => e;
export const js_els = (sel) => (pOrE) => () => pOrE.$$(sel);
export const js_el = (sel) => (pOrE) => () => pOrE.$(sel).then(assertExists);
export const js_innerText = (pOrE) => () =>
  pOrE.getProperty("innerText").then((prop) => prop.jsonValue());
export const js_getAttribute = (el) => (attr) => () =>
  el.evaluate((e, a) => e.getAttribute(a), attr).then(assertExists);
