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

export const js_keyOfByteInts = (bytes) =>
  b8sToB6s(...bytes)
    .map((n) => b6Char(n))
    .join("");

export const js_bytesOfInt = (mkByte) => (i) => {
  const b0 = (i >> (8 * 0)) & 255;
  const b1 = (i >> (8 * 1)) & 255;
  const b2 = (i >> (8 * 2)) & 255;
  const b3 = (i >> (8 * 3)) & 255;
  return [b0, b1, b2, b3].map(mkByte);
};

export const js_bytesOfString = (mkByte) => (s) => {
  const encoder = new TextEncoder();
  return [...encoder.encode(s).map(mkByte)];
};
