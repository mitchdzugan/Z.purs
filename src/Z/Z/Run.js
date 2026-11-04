export const js_ctx_provideInitial = (f) => () => f({ x: {}, c: {} });

export const js_ctx_at = (k) => (ctx) => {
  if (!ctx.x[k]) {
    console.log({ k });
    console.log(ctx);
  }
  return ctx.x[k].v;
};

export const js_ctx_runReaderAt = (k) => (v) => (useCtx) => (ctx) => {
  const w = ctx.x[k];
  const cWas = ctx.c[k] || 0;
  ctx.x[k] = { v, w };
  ctx.c[k] = cWas + 1;
  return useCtx(ctx).then((res) => {
    ctx.x[k] = w;
    if (!w) {
      delete ctx.x[k];
    }
    ctx.c[k]--;
    if (!cWas) {
      delete ctx.c[k];
    }
    return res;
  });
};

export const js_ctx_runExceptAt = (k) => (useCtx) => (ctx) => {
  const c = {};
  const prevC = ctx.c;
  ctx.c = c;
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
    ctx.c = prevC;
    for (const k in c) {
      for (let pops = c[k]; pops > 0; pops--) {
        ctx.x[k] = ctx.x[k].w;
      }
      if (!ctx.c[k]) {
        delete ctx.x[k];
      }
    }
    if (e instanceof LocalErrorClass) {
      return e.e;
    }
    throw e;
  };
  return js_ctx_runReaderAt(k)({ thrower, completer })(useCtx)(ctx);
};

export const js_ctx_unwrapErrorAt = (k) => (ctx) => ctx.x[k].v.completer;

export const js_ctx_throwAt = (k) => (e) => (ctx) => ctx.x[k].v.thrower(e);
