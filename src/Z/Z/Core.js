export const js_JsAny = (a) => a;
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
