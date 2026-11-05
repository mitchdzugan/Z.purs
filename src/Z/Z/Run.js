export const js_ctx_provideInitial = (f) => () => f({});

export const js_ctx_at = (k) => (ctx) => {
  if (!ctx[k]) {
    console.log({ k });
    console.log(ctx);
  }
  return ctx[k].v;
};

export const js_ctx_runReaderAt = (map) => (k) => (v) => (useCtx) => {
  let w;
  let ctx;
  const mapper = (res) => {
    ctx[k] = w;
    if (!w) {
      delete ctx[k];
    }
    return res;
  };
  return map(mapper)((_ctx) => {
    ctx = _ctx;
    w = ctx[k];
    ctx[k] = { v, w };
    return useCtx(ctx);
  });
};

let cExcCount = 0;

export const js_ctx_runExceptAt = (map) => (k) => (useCtx) => (ctx) => {
  cExcCount++;
  // console.log("CLONING EXCEPT", { cExcCount, k });
  const init = { ...ctx };
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
      // console.log("APPLYING CLONED EXCEPT", { cExcCount, k });
      Object.assign(ctx, init);
      return e.e;
    }
    throw e;
  };
  return js_ctx_runReaderAt(map)(k)({ thrower, completer })(useCtx)(ctx);
};

export const js_ctx_unwrapErrorAt = (k) => (ctx) => ctx[k].v.completer;

export const js_ctx_throwAt = (k) => (e) => (ctx) => ctx[k].v.thrower(e);
