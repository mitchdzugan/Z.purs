Error.stackTraceLimit = Infinity;

function getCleanStack() {
  const traceTarget = {};
  Error.captureStackTrace(traceTarget, getCleanStack);
  return traceTarget.stack
    .split("\n")
    .slice(1)
    .map((s) => s.trim())[2];
}
const colors = {
  reset: "\x1b[0m",
  red: "\x1b[31m",
  green: "\x1b[32m",
  yellow: "\x1b[33m",
  blue: "\x1b[34m",
  magenta: "\x1b[35m",
  cyan: "\x1b[36m",
  white: "\x1b[37m",
  gray: "\x1b[90m",
  Bred: "\x1b[91m",
  Bgreen: "\x1b[92m",
  Byellow: "\x1b[93m",
  Bblue: "\x1b[94m",
  Bmagenta: "\x1b[95m",
  Bcyan: "\x1b[96m",
  Bwhite: "\x1b[97m",
};

export const js_consoleFn = (prop) => (preface) => (args) => {
  const stack = getCleanStack();
  const stackStr = stack ? ` ${colors.yellow}${stack.substring(3)}` : "";
  const fn = console[prop];
  return () => {
    fn(
      `${colors.magenta}${preface}${stackStr}`,
      `${colors.gray}[`,
      colors.reset,
    );
    console.group();
    fn(...args);
    console.groupEnd();
    fn(`${colors.gray}]`, colors.reset);
  };
};
