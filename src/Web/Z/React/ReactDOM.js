import { createRoot } from "react-dom/client";

export const js_renderIn = (reactEl) => (domEl) => () =>
  new Promise((resolve, reject) => {
    try {
      resolve(createRoot(domEl).render(reactEl));
    } catch (error) {
      reject(error);
    }
  });
