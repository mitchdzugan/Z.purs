import { hydrate } from "preact";

export const js_renderIn = (reactEl) => (domEl) => () =>
  new Promise((resolve, reject) => {
    try {
      resolve(hydrate(reactEl, domEl));
    } catch (error) {
      reject(error);
    }
  });

export const js_el_gToWeb = (g) => g
export const js_el_webToG = (g) => g
export const js_event_gToWeb = (g) => g
export const js_event_webToG = (g) => g
export const js_animationFramePromise = () => new Promise(
  (resolve) => window.requestAnimationFrame(() => resolve())
);