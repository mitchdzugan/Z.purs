import * as ini from "ini";

export const js_JsAny = (a) => a;
export const js_JsAnyToForeign = (a) => a;
export const js_ForeignToJson = (a) => a;
export const js_jsonStr = (j) => `${j}`;
export const js_simpleHash = (str) => {
  let hash = 0;
  for (let i = 0; i < str.length; i++) {
    const char = str.charCodeAt(i);
    hash = (hash << 5) - hash + char;
    hash |= 0;
  }
  return hash;
};
export const js_removeNils = (o) => {
  const res = {};
  for (const k in o) {
    const v = o[k];
    if (v === null || v === undefined) {
      continue;
    }
    res[k] = v;
  }
  return res;
};
export const js_runDeferred = (f) => f();
export const js_arrWithInd = (mkTuple) => (a) => a.map((v, i) => mkTuple(i)(v));
export const js_iniStringifyJson = (j) => ini.stringify(j);
export function js_zScoreToPercentile(z) {
  if (z < -6.5) return 0.0;
  if (z > 6.5) return 1.0;
  let factK = 1;
  let sum = 0;
  let term = 1;
  let k = 0;
  let loopStop = Math.exp(-23);
  while (Math.abs(term) > loopStop) {
    term =
      (((0.3989422804 * Math.pow(-1, k) * Math.pow(z, k)) /
        (2 * k + 1) /
        Math.pow(2, k)) *
        Math.pow(z, k + 1)) /
      factK;
    sum += term;
    k++;
    factK *= k;
  }
  return (sum + 0.5) * 100;
}
