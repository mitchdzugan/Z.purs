import { SlippiGame } from "@slippi/slippi-js";

export const js_gameOfBuffer = (b) => new SlippiGame(b);
export const js_stats = (g) => {
  const stats = g.getStats();
  // console.log({ stats });
  return stats;
};
export const js_settings = (g) => {
  const settings = g.getSettings();
  // console.log({ settings });
  return settings;
};

export const js_meta = (g) => {
  const metadata = g.getMetadata();
  // console.log({ metadata });
  return metadata;
};

export const js_startAt = (n) => (j) => (g) => {
  try {
    const metadata = g.getMetadata() || {};
    const startAt = metadata.startAt || "";
    const dateVal = new Date(startAt).valueOf();
    return Number.isNaN(dateVal) ? n : j(dateVal);
  } catch (_e) {
    return n;
  }
};
