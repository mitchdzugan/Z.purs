class ThenableSync {
  constructor(v) {
    this.v = v;
  }

  then(f) {
    const res = f(this.v);
    return res instanceof Promise || res instanceof ThenableSync
      ? res
      : new ThenableSync(res);
  }
}

const Thenable = (v) => new ThenableSync(v);

const ensurePromise = (t) => (t instanceof Promise ? t : Promise.resolve(t.v));

export const js_thenableR_map = (f) => (thR) => (r) => thR(r).then(f);

export const js_thenableR_apply = (fThnblR) => (vThnblR) => (r) =>
  fThnblR(r).then((f) => vThnblR(r).then(f));

export const js_thenableR_pure = (v) => () => Thenable(v);

export const js_thenableR_ask = (r) => Thenable(r);

export const js_thenableR_bind = (vThnblR) => (fvThnblR) => (r) =>
  vThnblR(r).then((v) => fvThnblR(v)(r));

const tailRecM_async = async (s0P, useL, useD, ref, useS, nThR, getA, r) => {
  let isInitial = true;
  while (ref.length == 0) {
    const proc = (step) => useS(useL)(useD)(step)();
    if (isInitial) {
      proc(await s0P);
    } else {
      const thnbl = nThR(getA())(r);
      proc(thnbl instanceof Promise ? await thnbl : thnbl.r);
    }
    isInitial = false;
  }
  return ref[0];
};

export const js_thenableR_tailRecM = (useStep) => (nThR) => (a1) => (r) => {
  let a = a1;
  let ref = [];
  const useL = (ax) => () => (a = ax);
  const useD = (res) => () => (ref = [res]);
  while (ref.length == 0) {
    const thnbl = nThR(a)(r);
    if (thnbl instanceof Promise) {
      return tailRecM_async(thnbl, useL, useD, ref, useStep, nThR, () => a, r);
    }
    useStep(useL)(useD)(thnbl.r)();
  }
  return ref[0];
};

export const js_thenableR_sync = (r) => (thnblR) => thnblR(r).v;

export const js_thenableR_async = (r) => (thnblR) => () =>
  ensurePromise(thnblR(r));

export const js_thenableR_runA = (r) => (onDone) => (onE) => (thnblR) => () =>
  ensurePromise(thnblR(r))
    .then((v) => onDone(v)())
    .catch((e) => onE(e)());

export const js_thenableR_asyncFromEff = (eff) => async () => eff();

export const js_thenableR_asyncFromEffPromise = (effP) => effP;

export const js_thenableR_parallel = (thnblRs) => (r) => {
  let isAllSync = true;
  const thnbls = thnblRs.map((thnblR) => {
    const thnbl = thnblR(r);
    isAllSync &&= !(thnbl instanceof Promise);
    return ensurePromise(thnbl);
  });
  return isAllSync ? Thenable(thnbls.map(({ v }) => v)) : Promise.all(thnbls);
};

class NonError extends Error {
  name = "NonError";
  constructor(cause) {
    super("thrown value is not an instance of Error", { cause });
  }
}

export const js_thenableR_try = (Left) => (Right) => (thR) => async (r) => {
  try {
    return Right(await ensurePromise(thR(r)));
  } catch (caught) {
    return Left(caught instanceof Error ? caught : new NonError(caught));
  }
};
