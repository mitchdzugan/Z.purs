export const js_dict_fresh = (toKeys) => () => ({ toKeys, data: {}, size: 0 });

export const js_dict_freeze = (dict) => {
  dict.frozen = true;
  return dict;
};

const effEnsureUnfrozen = (f) => (dict) => () => {
  if (dict.frozen) throw new Error("operating on frozen dict");
  return f(dict);
};

export const js_dict_insert = (k) => (v) =>
  effEnsureUnfrozen((dict) => {
    const keys = dict.toKeys(k);
    function insLoop(data, keysInd) {
      const keysEl = keys[keysInd];
      if (!keysEl) {
        const added = data.f == 1 ? 0 : 1;
        data.f = 1;
        data.v = v;
        return added;
      }
      const { s, k } = keysEl;
      const dataSWas = data[s];
      data[s] = dataSWas || [{}, 0, k];
      const added = insLoop(data[s][0], keysInd + 1);
      data[s][1] += added;
      return !dataSWas && added ? 1 : 0;
    }
    const added = insLoop(dict.data, 0);
    dict.size += added;
  });

const dataDig = (keys) => (dict) => (defaultVal) => (completer) => {
  let data = dict.data;
  let size = dict.size;
  for (const s of keys) {
    [data, size] = data[s] || [];
    if (!data) {
      return defaultVal;
    }
  }
  return completer(data, size);
};

function entriesDataLoop(mapFst, mkTuple, data) {
  if (data.f == 1) {
    return [true, [data.v]];
  } else {
    const wrappedEls = Object.values(data).flatMap((next) => {
      const [wasHead, els] = entriesDataLoop(mapFst, mkTuple, next[0]);
      return els.map(wasHead ? mkTuple(next[2]) : mapFst(mkTuple(next[2])));
    });
    return [false, wrappedEls];
  }
}

export const js_dict_entriesAt =
  (mapFst) => (mkTuple) => (pkeys) => (dict) => () =>
    dataDig(pkeys)(dict)([])(
      (data) => entriesDataLoop(mapFst, mkTuple, data)[1],
    );

export const js_dict_sizeAt = (pkeys) => (dict) => () =>
  dataDig(pkeys)(dict)(0)((_data, size) => size);

export const js_dict_keysAt = (pkeys) => (dict) => () =>
  dataDig(pkeys)(dict)([])((data) =>
    data.f == 1 ? [undefined] : Object.values(data).map((dataAt) => dataAt[2]),
  );
