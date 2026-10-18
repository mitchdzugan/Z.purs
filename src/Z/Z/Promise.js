export const js_promiseMap = (f) => async (pv) => {
  const v = await pv;
  return f(v);
};

export const js_promiseApply = (pf) => async (pv) => {
  const f = await pf;
  const v = await pv;
  return f(v);
};

export const js_promisePure = (v) => Promise.resolve(v);

export const js_promiseBind = (pv) => async (fp) => {
  const v = await pv;
  return await fp(v);
};

class NonError extends Error {
  constructor(cause) {
    super("thrown value is not an instance of Error", { cause });
  }
}

export const js_promiseTry = (Left) => (Right) => async (pv) => {
  try {
    return Right(await pv);
  } catch (caught) {
    const error = (() => {
      if (caught instanceof Error) {
        return caught;
      }
      return new NonError(caught);
    })();
    return Left(error);
  }
};

export const js_promiseFromEff = async (eff) => {
  return eff();
};
