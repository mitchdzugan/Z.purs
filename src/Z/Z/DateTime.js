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
export const js_makeDate = (year) => (month) => (day) => (timezoneOpts) => {
  const utcDate = new Date(Date.UTC(year, month - 1, day));
  const mkParts = (tzOpts = {}) =>
    new Intl.DateTimeFormat("en-US", {
      timeZone: tzOpts.timeZone || "UTC",
      year: "numeric",
      month: "numeric",
      day: "numeric",
      hour: "numeric",
      minute: "numeric",
      second: "numeric",
      hour12: false,
    }).formatToParts(utcDate);
  const utcParts = mkParts();
  const tzParts = mkParts(timezoneOpts);
  const parseParts = (p) =>
    new Date(
      p.find((c) => c.type === "year").value,
      p.find((c) => c.type === "month").value - 1,
      p.find((c) => c.type === "day").value,
      p.find((c) => c.type === "hour").value,
      p.find((c) => c.type === "minute").value,
      p.find((c) => c.type === "second").value,
    ).getTime();
  const msDelta = parseParts(utcParts) - parseParts(tzParts);
  return utcDate.getTime() + msDelta;
};
