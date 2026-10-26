import { hydrate } from "preact";

export const js_renderIn = (reactEl) => (domEl) => () =>
  new Promise((resolve, reject) => {
    try {
      resolve(hydrate(reactEl, domEl));
    } catch (error) {
      reject(error);
    }
  });

export const js_gToWeb = (g) => g
export const js_webToG = (g) => g
export const js_animationFramePromise = () => new Promise(
  (resolve) => window.requestAnimationFrame(() => resolve())
);