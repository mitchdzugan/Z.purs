import { hydrate } from "preact";

export const js_renderIn = (reactEl) => (domEl) => () =>
  new Promise((resolve, reject) => {
    try {
      resolve(hydrate(reactEl, domEl));
    } catch (error) {
      reject(error);
    }
  });
