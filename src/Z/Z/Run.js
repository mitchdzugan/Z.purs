export const js_ctx_runSync = (useCtx) => useCtx({ x: {} });
export const js_ctx_runAsync = (useCtx) => async () => useCtx({ x: {} });
export const js_ctx_askAt = (k) => (ctx) => ctx[k][0];
export const js_ctx_throwAt = (k) => (e) => (ctx) => ctx[k][0](e);
export const js_ctx_runReaderAt = (k) => (v) => (useCtx) => (ctx) => {
  const was = ctx[k];
  ctx[k] = [v];
  const res = useCtx(ctx);
  if (!was) {
    delete ctx[k];
  } else {
    ctx[k] = was;
  }
  return res;
};
export const js_ctx_runExceptAt = (L) => (R) => (k) => (useCtx) => (ctx) => {
  const LocalErrorClass = class LocalError {
    constructor(e) {
      this.e = e;
    }
  };
  const thrower = (e) => {
    throw new LocalErrorClass(e);
  };
  try {
    return R(js_ctx_runReaderAt(k)(thrower)(useCtx)(ctx));
  } catch (e) {
    if (e instanceof LocalErrorClass) {
      return L(e.e);
    } else {
      throw e;
    }
  }
};
export const js_ctx_liftFuture = (future) => (useFuture) => async (ctx) => {
  const v = await future();
  console.log({ v, ctx, useFuture });
  return useFuture(v)(ctx);
};
