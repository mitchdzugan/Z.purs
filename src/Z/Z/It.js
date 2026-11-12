export const js_rawIt_next = (rawIt) => () => rawIt.next();

export const js_it_map = (f) => (mkIt) =>
  function* () {
    const it = mkIt();
    for (const el of it) {
      yield f(el);
    }
  };

export const js_it_withInd = (mkTuple) => (mkIt) =>
  function* () {
    const it = mkIt();
    let i = 0;
    for (const el of it) {
      yield mkTuple(i)(el);
      i++;
    }
  };

export const js_it_ofArray = (arr) =>
  function* () {
    for (const el of arr) {
      yield el;
    }
  };

export const js_it_ofObj = (mkTuple) => (obj) =>
  function* () {
    for (const k in obj) {
      yield mkTuple(k)(obj[k]);
    }
  };

export const js_it_ofList = (useList) => (list) =>
  function* () {
    let carry = list;
    let isDone = false;
    let nextEl;
    while (true) {
      useList(() => (isDone = true))((el) => (rest) => () => {
        carry = rest;
        nextEl = el;
      })(carry)();
      if (isDone) {
        break;
      }
      yield nextEl;
    }
  };

export const js_it_takeSome = (useJust) => (mkIt) =>
  function* () {
    const it = mkIt();
    for (const elOr_ of it) {
      const ref = [];
      useJust((a) => () => ref.push(a))(elOr_)();
      for (const el of ref) {
        yield el;
      }
    }
  };

export const js_it_toArray = (mkIt) => Array.from(mkIt());

export const js_it_toObj = (mkIt) => Object.fromEntries(mkIt());

export const js_it_toObjEls = (mkTuple) => (snd) => (mkIt) => {
  const it = mkIt();
  const res = {};
  for (const [k, v] of it) {
    if (!res[k]) {
      res[k] = mkTuple(v)([]);
    } else {
      snd(res[k]).push(v);
    }
  }
  return res;
};

export const js_obj_entry = (k) => (v) => [k, v];
