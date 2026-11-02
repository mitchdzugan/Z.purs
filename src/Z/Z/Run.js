export const js_ctx_default = {};

export const js_ctx_at = (k) => (ctx) => ctx[k][0];

export const js_ctx_runReaderAt = (k) => (v) => (useCtx) => (_ctx) => {
  const ctx = _ctx == js_ctx_default ? {} : js_ctx_default;
  const was = ctx[k];
  ctx[k] = [v];
  return useCtx(ctx).then((res) => {
    ctx[k] = was;
    if (!was) {
      delete ctx[k];
    }
    return res;
  });
};

export const js_ctx_runExceptAt = (k) => (useCtx) => (ctx) => {
  const LocalErrorClass = class LocalError extends Error {
    constructor(e) {
      super("");
      this.e = e;
    }
  };
  const thrower = (e) => {
    throw new LocalErrorClass(e);
  };
  const completer = (e) => {
    if (e instanceof LocalErrorClass) {
      return e.e;
    }
    throw e;
  };
  return js_ctx_runReaderAt(k)({ thrower, completer })(useCtx)(ctx);
};

export const js_ctx_unwrapErrorAt = (k) => (ctx) => ctx[k][0].completer;

export const js_ctx_throwAt = (k) => (e) => (ctx) => ctx[k][0].thrower(e);
