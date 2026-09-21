export const js_timeout = (ms) => () =>
  new Promise((res) => setTimeout(() => res(), ms));
