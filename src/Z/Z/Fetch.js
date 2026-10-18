export const js_fetch = (url) => (options) => fetch(url, options);

export const js_fetchRes_json = (fetchRes) => fetchRes.json();

export const js_fetchRes_text = (fetchRes) => fetchRes.text();
