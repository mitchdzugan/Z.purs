export const js_parseStringToJson = (L) => (R) => (s) => {
  try {
    return R(JSON.parse(s));
  } catch (e) {
    return L(e);
  }
};
