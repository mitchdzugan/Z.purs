export const js_newBoxed = (val) => {
  const cache = {};
  return { val, cache };
};
export const js_boxedVal = (boxed) => boxed.val;
export const js_boxedGetCachedOrEval =
  (methodId) => (paramId) => (mk) => (boxed) => {
    boxed.cache[methodId] ||= {};
    if (!boxed.cache[methodId][paramId]) {
      boxed.cache[methodId][paramId] = [mk()];
    }
    return boxed.cache[methodId][paramId][0];
  };
