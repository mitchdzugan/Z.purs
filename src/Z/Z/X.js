export const js_consoleFn = (prop) => (preface) => (args) => () =>
  console[prop](preface, args);
