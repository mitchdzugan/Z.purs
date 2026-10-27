export const js_futureMap = (f) => (pv) => async () => {
  const v = await pv();
  return f(v);
};

export const js_futureApply = (pf) => (pv) => async () => {
  const f = await pf();
  const v = await pv();
  return f(v);
};

export const js_futurePure = (v) => () => Promise.resolve(v);

export const js_futureBind = (pv) => (fp) => async () => {
  const v = await pv();
  return await fp(v)();
};

class NonError extends Error {
  constructor(cause) {
    super("thrown value is not an instance of Error", { cause });
  }
}

export const js_futureTry = (Left) => (Right) => (pv) => async () => {
  try {
    return Right(await pv());
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

export const js_futureFromEff = (eff) => async () => eff();

export const js_futureRun = (onDone) => (onError) => (p) => () =>
  p()
    .then((v) => onDone(v)())
    .catch((e) => onError(e)());

export const js_futureFromEffPromise = (effPromise) => effPromise;

export const js_futureTailRecM = (useStep) => (nextF) => (a1) => async () => {
  let a = a1;
  let resRef = [];
  while (resRef.length == 0) {
    const step = await nextF(a)();
    const useLoop = (ax) => () => (a = ax);
    const useDone = (res) => () => (resRef = [res]);
    useStep(useLoop)(useDone)(step)();
  }
  return resRef[0];
};

export const js_futureParallel = (pvs) => async () => {
  return await Promise.all(pvs.map((pv) => pv()));
};

export const js_futureNever = () => new Promise(() => {});
