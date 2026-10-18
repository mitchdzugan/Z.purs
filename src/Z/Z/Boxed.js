export const js_newBoxed = (val) => {
    const cache = {};
    return { val, cache }
}
export const js_boxedVal = (boxed) => boxed.val;
export const js_boxedGetCachedOrEval = (fnAndArgId) => (mk) => (boxed) => {
    const cached = boxed.cache[fnAndArgId];
    if (cached) {
        return cached[0];
    }
    const res = mk();
    boxed.cache[fnAndArgId] = [res];
    return res;
};