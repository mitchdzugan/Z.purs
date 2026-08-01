function withInd(arr) {
  return arr.map((e, i) => [e, i]);
}

function quot(n, d) {
  return Math.trunc(n / d);
}

function chunkArr(a, n) {
  const res = [];
  let nextChunk = [];
  for (const [e, i] of withInd(a)) {
    nextChunk.push(e);
    if (quot(i, n) !== quot(i + 1, n)) {
      res.push(nextChunk);
      nextChunk = [];
    }
  }
  if (nextChunk.length > 0) {
    res.push(nextChunk);
  }
  return res;
}

const mLookup = [
  "0",
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "9",
  "a",
  "b",
  "c",
  "d",
  "e",
  "f",
  "g",
  "h",
  "i",
  "j",
  "k",
  "l",
  "m",
  "n",
  "o",
  "p",
  "q",
  "r",
  "s",
  "t",
  "u",
  "v",
  "w",
  "x",
  "y",
  "z",
  "A",
  "B",
  "C",
  "D",
  "E",
  "F",
  "G",
  "H",
  "I",
  "J",
  "K",
  "L",
  "M",
  "N",
  "O",
  "P",
  "Q",
  "R",
  "S",
  "T",
  "U",
  "V",
  "W",
  "X",
  "Y",
  "Z",
  "-",
];
function b6Char(n) {
  return mLookup[n] || "_";
}

function b8sToB6s(...b8s) {
  const res = [];
  const incoming = [...b8s];
  incoming.reverse();
  for (const chunk of chunkArr(incoming, 3)) {
    const b0 = Math.pow(256, 0) * (chunk[0] || 0);
    const b1 = Math.pow(256, 1) * (chunk[1] || 0);
    const b2 = Math.pow(256, 2) * (chunk[2] || 0);
    let v = b0 + b1 + b2;
    for (let j = 0; j < 4; j++) {
      res.push(v % 64);
      v = Math.floor(v / 64);
    }
  }
  res.reverse();
  let start = 0;
  while (start < 4 && !res[start]) {
    start++;
  }
  return res.slice(start);
}

export function js_keyOfStr(s) {
  const encoder = new TextEncoder();
  return b8sToB6s(...encoder.encode(s))
    .map((n) => b6Char(n))
    .join("");
}

export function js_keyOfInt(i) {
  if (i >= 0 && i < 100) {
    return js_keyOfStr(`${i}`);
  }
  const b6_0 = (i >> (6 * 0)) & 63;
  const b6_1 = (i >> (6 * 1)) & 63;
  const b6_2 = (i >> (6 * 2)) & 63;
  const b6_3 = (i >> (6 * 3)) & 63;
  const b6_4 = (i >> (6 * 4)) & 63;
  const b6_5 = (i >> (6 * 5)) & 63;
  return [b6_0, b6_1, b6_2, b6_3, b6_4, b6_5].map((n) => b6Char(n)).join("");
}

export function js_keyOfAKeys(a) {
  return a.map((s) => `${s.length}${s}`).join("");
}

export function js_keyOfBytes(bytes) {
  return b8sToB6s(...bytes)
    .map((n) => b6Char(n))
    .join("");
}
