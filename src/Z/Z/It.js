export const js_it_process = (useS) => (onN) => (onD) => (init) => (mkIt) => {
  let carry = init;
  let resRef = [];
  const useLoop = (updateCarry) => () => (carry = updateCarry);
  const useDone = (res) => () => (resRef = [res]);
  const it = mkIt();
  for (const el of it) {
    const step = onN(carry)(el);
    useS(useLoop)(useDone)(step)();
    if (resRef.length > 0) {
      return resRef[0];
    }
  }
  return onD(carry);
};
