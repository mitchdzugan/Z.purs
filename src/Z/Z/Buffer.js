export const js_ofArrayBuffer = (b) => b;
export const js_sha256OfBuffer = (b) => async () => {
  const hashA = await js_sha256ArrOfBuffer(b)();
  return Array.from(hashA)
    .map((b) => b.toString(16).padStart(2, "0"))
    .join("");
};

export const js_sha256ArrOfBuffer = (b) => async () => {
  const hashB = await crypto.subtle.digest("SHA-256", b);
  const hashA = new Uint8Array(hashB);
  return Array.from(hashA);
};
