class ThenableSync {
  constructor(v) {
    this.v = v;
  }

  then(f) {
    const res = f(this.v);
    return res instanceof Promise ? res : Thenable(res);
  }
}

const _th = (th) => (f) =>
  th.then((a) => {
    const inner = f(a);
    return inner instanceof ThenableSync ? inner.v : inner;
  });

const Thenable = (v) => new ThenableSync(v);

const ensurePromise = (t) => (t instanceof Promise ? t : Promise.resolve(t.v));

export const js_thenableR_map = (f) => (thR) => (r) => _th(thR(r))(f);

export const js_thenableR_apply = (fThnblR) => (vThnblR) => (r) =>
  _th(fThnblR(r))((f) => _th(vThnblR(r))(f));

export const js_thenableR_pure = (v) => () => Thenable(v);

export const js_thenableR_ask = (r) => Thenable(r);

export const js_thenableR_bind = (vThnblR) => (fvThnblR) => (r) =>
  _th(vThnblR(r))((v) => fvThnblR(v)(r));

const tailRecM_async = async (s0P, ref, useS, nThR, getA, r) => {
  let isInitial = true;
  while (ref.length == 0) {
    if (isInitial) {
      useS(await s0P)();
    } else {
      const thnbl = nThR(getA())(r);
      useS(thnbl instanceof Promise ? await thnbl : thnbl.v)();
    }
    isInitial = false;
  }
  return ref[0];
};

export const js_thenableR_tailRecM = (useStep) => (nThR) => (a1) => (r) => {
  let a = a1;
  let ref = [];
  const useL = (ax) => () => (a = ax);
  const useD = (res) => () => (ref[0] = res);
  while (ref.length == 0) {
    const thnbl = nThR(a)(r);
    if (thnbl instanceof Promise) {
      return tailRecM_async(thnbl, ref, useStep(useL)(useD), nThR, () => a, r);
    }
    useStep(useL)(useD)(thnbl.v)();
  }
  return Thenable(ref[0]);
};

export const js_thenableR_sync = (r) => (thnblR) => thnblR(r).v;

export const js_thenableR_runA = (r) => (onDone) => (onE) => (thnblR) => () => {
  _th(ensurePromise(thnblR(r)))((v) => onDone(v)()).catch((e) => onE(e)());
};

export const js_thenableR_asyncFromEff = (eff) => async () => eff();

export const js_thenableR_asyncFromEffPromise = (effP) => effP;

export const js_thenableR_parallel = (thnblRs) => (r) => {
  let isSync = true;
  const ths = thnblRs.map((thnblR) => {
    const thnbl = thnblR(r);
    isSync &&= !(thnbl instanceof Promise);
    return { p: ensurePromise(thnbl), s: thnbl };
  });
  const els = isSync ? ths.map((e) => e.s.v) : ths.map((e) => e.p);
  return (isSync ? Thenable : Promise.all)(els);
};

class NonError extends Error {
  name = "NonError";
  constructor(cause) {
    super("thrown value is not an instance of Error", { cause });
  }
}

export const js_thenableR_try = (Left) => (Right) => (thR) => (r) => {
  const toL = (e) => Left(e instanceof Error ? e : new NonError(e));
  try {
    const res = _th(thR(r))(Right);
    return res instanceof Promise ? res.catch(toL) : res;
  } catch (caught) {
    return Thenable(toL(caught));
  }
};
