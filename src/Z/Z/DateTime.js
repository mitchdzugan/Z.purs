const getFormattedImpl = (mw) => (baseOpts) => (ms) => (opts) => {
  const date = new Date(ms);
  const formatter = new Intl.DateTimeFormat("en-US", {
    ...baseOpts,
    ...opts,
  });
  return mw(formatter.format(date));
};
// const getFormatted = getFormattedImpl((s) => s);
const getFormattedInt = getFormattedImpl((s) => parseInt(s, 10));
export const js_effNow = () => Date.now();
export const js_dateMonth = getFormattedInt({ month: "numeric" });
export const js_dateYear = getFormattedInt({ year: "numeric" });
export const js_dateDay = getFormattedInt({ day: "numeric" });
