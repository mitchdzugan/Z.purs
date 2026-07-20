export function js_getStack() {
  const traceTarget = {};
  Error.captureStackTrace(traceTarget, js_getStack);
  const s = traceTarget.stack
    .split("\n")
    .slice(1)
    .map((s) => s.trim());
  return s[2].replaceAll("<anonymous> ", "").trim();
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

const cl = "﹃";
const cr = "﹄";

export const js_consoleFn = (prop) => (src) => (args) => {
  const stackStr = src ? ` ${colors.gray}${src.substring(3)}` : "";
  const fn = console[prop];
  const propColor = {
    log: colors.cyan,
    warn: colors.Byellow,
    error: colors.red,
  }[prop];
  const propLabel = {
    log: "logInfo",
    warn: "logWarning",
    error: "logError",
  }[prop];
  const nowMS = Date.now();

  const divTime = (curr, d) => [curr % d, Math.floor(curr / d)];

  const [ms, nowS] = divTime(nowMS, 1000);
  const [s, nowM] = divTime(nowS, 60);
  const [m, nowH] = divTime(nowM, 60);
  const h = (nowH + 19) % 24;
  const mPad = m < 10 ? `0${m}` : `${m}`;
  const sPad = s < 10 ? `0${s}` : `${s}`;
  const msPad = s < 10 ? `00${ms}` : ms < 100 ? `0${ms}` : `${ms}`;

  const l1Parts = [
    colors.magenta,
    "χ::",
    propColor,
    propLabel,
    stackStr,
    colors.blue,
    ` ${h}:${mPad}:${sPad}.${msPad} `,
    colors.magenta,
    cl,
    colors.reset,
  ];
  return () => {
    fn(l1Parts.join(""));
    console.group();
    fn(...args);
    console.groupEnd();
    fn(`${colors.magenta}${cr}`, colors.reset);
  };
};

export const js_timeout = (ms) => () =>
  new Promise((res) => setTimeout(() => res(), ms));
