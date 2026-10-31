const stack = () => null;
const push = (stack, v) => ({ v, r: stack });
const peek = (stack) => stack.v;
const pop = (stack) => stack.r;

export const js_r_pure = (v) => () => v;

export const js_r_map = (f) => (r) => (x) => {
  const v = r(x);
  return f(v);
};

export const js_r_apply = (rf) => (rv) => (x) => {
  const f = rf(x);
  const v = rv(x);
  return f(v);
};

export const js_r_bind = (rv) => (fr) => (x) => {
  const v = rv(x);
  return fr(v)(x);
};

export const js_r_tailRecM = (useStep) => (nextR) => (a1) => (x) => {
  let a = a1;
  let resRef = [];
  while (resRef.length == 0) {
    const step = nextR(a)(x);
    const useLoop = (ax) => () => (a = ax);
    const useDone = (res) => () => (resRef = [res]);
    useStep(useLoop)(useDone)(step)();
  }
  return resRef[0];
};
