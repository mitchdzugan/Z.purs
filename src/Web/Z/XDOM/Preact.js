// import { createRoot } from "react-dom/client";
import { hydrate } from "preact";

export const js_renderIn = (reactEl) => (domEl) => () =>
  new Promise((resolve, reject) => {
    try {
      // resolve(createRoot(domEl).render(reactEl));
      resolve(hydrate(reactEl, domEl));
    } catch (error) {
      reject(error);
    }
  });
