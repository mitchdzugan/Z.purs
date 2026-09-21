export const js_consoleOut = (unit) => (consoleKey) => (loggable) => () => {
  console[consoleKey](loggable);
  return unit;
};
