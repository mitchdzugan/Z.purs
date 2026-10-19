import { stringify as iniStringify } from "ini";

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
export const js_arrWithInd = (mkTuple) => (a) => a.map((v, i) => mkTuple(i)(v));
export const js_iniStringifyJson = (j) => iniStringify(j);
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
const ROMAN_NUMERALS = [
  {v: 1000, c: "M"},
  {v: 900, c: "CM"},
  {v: 500, c: "D"},
  {v: 400, c: "CD"},
  {v: 100, c: "C"},
  {v: 90, c: "XC"},
  {v: 50, c: "L"},
  {v: 40, c: "XL"},
  {v: 10, c: "X"},
  {v: 9, c: "IX"},
  {v: 5, c: "V"},
  {v: 4, c: "IV"},
  {v: 1, c: "I"},
]
export function js_toRomanNumeral(rawInput) {
  function impl(input) {
    let v = input;
    let i = 0;
    let res = "";
    while (v > 0) {
      const num = ROMAN_NUMERALS[i];
      while (v >= num.v) {
        v -= num.v;
        res += num.c;
      }
      i++;
    }
    return res;
  }
  if (rawInput == 0) {
    return "_"
  } else if (rawInput < 0) {
    return "-" + impl(rawInput * -1);
  } else {
    return impl(rawInput)
  }
}