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

export const js_it_toArray = (mkIt) => [...mkIt()];

export const js_it_toObj = (mkIt) => Object.fromEntries(mkIt());

export const js_obj_entry = (k) => (v) => [k, v];

export const js_arrayPartial_new = () => [];

export const js_arrayPartial_cons = (p) => (el) => () => {
  p.push(el);
};
