let nextId = 1;
export const js_getMethodId = (f) => {
  if (!f.idRef) {
    const id = nextId;
    nextId++;
    f.idRef = [id];
  }
  return f.idRef[0];
};
