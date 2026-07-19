export const js_consoleFn = (prop) => (preface) => (args) => () =>
  console[prop]("\x1b[35m%s\x1b[0m", preface, "\x1b[0m", args);
