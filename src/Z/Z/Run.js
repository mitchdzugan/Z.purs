export const js_ctx_provideInitial = (f) => () => f({ x: {}, s: { c: 0 } });

export const js_ctx_provideInitialFrom = (ctx) => (f) => () => f(ctx);

export const js_ctx_at = (k) => (ctx) => ctx.x[k].v;

export const js_ctx_clone = (cloners) => (ctx) => {
  let maxC = 0;
  let maxK = null;
  const resX = {};
  const ctxX = ctx.x;
  for (const k in cloners) {
    const { c, v } = ctxX[k];
    if (c > maxC) {
      maxC = c;
      maxK = k;
    }
    resX[k] = cloners[k](v);
  }
  return {
    x: resX,
    s: { c: maxC, k: maxK },
  };
};

export const js_ctx_initReaderAt = (ctx) => (k) => (v) => (m) => {
  const cs = ctx.s;
  const c = cs.c + 1;
  const cx = ctx.x[k];
  ctx.x[k] = { c, v, w: cx };
  ctx.s = { c, k, w: cs };
  return m;
};

export const js_ctx_completeReaderAt = (ctx) => (k) => (pureRes) => {
  ctx.s = ctx.s.w;
  ctx.x[k] = ctx.x[k].w;
  if (!ctx.x[k]) {
    delete ctx.x[k];
  }
  return pureRes;
};

export const js_ctx_mkExceptAt = (ctx) => {
  const localC = ctx.s.c + 1;
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
      while (ctx.s.c > localC) {
        ctx.x[ctx.s.k] = ctx.x[ctx.s.k].w;
        ctx.s = ctx.s.w;
      }
      return e.e;
    }
    throw e;
  };
  return { thrower, completer };
};

export const js_ctx_unwrapErrorAt = (k) => (ctx) => ctx.x[k].v.completer;

export const js_ctx_throwAt = (k) => (e) => (ctx) => ctx.x[k].v.thrower(e);
