const clone = (u) => new URL(u.href);

export const fromStringImpl = (s) => {
  try {
    return new URL(s);
  } catch {
    return null;
  }
};

export const hashImpl = (u) => u.hash;

export const hostImpl = (u) => u.hostname;

export const hrefImpl = (u) => u.href;

export const originImpl = (u) => u.origin;

export const passwordImpl = (u) => u.password;

export const pathnameImpl = (u) => u.pathname;

export const portImpl = (u) => u.port;

export const protocolImpl = (u) => u.protocol;

export const searchImpl = (u) => u.search;

export const relativeImpl = (u) =>
  `${u.pathname || ""}${u.search || ""}${u.hash || ""}`;

export const searchParamsImpl = (u) => u.searchParams;

export const usernameImpl = (u) => u.username;

export const queryKeysImpl = (u) => Array.from(u.searchParams.keys());

export const queryLookupImpl = (k) => (u) => u.searchParams.getAll(k);

export const querySetAllImpl = (qs) => (u) => {
  const u_ = clone(u);
  u_.search = "";
  qs.forEach(({ k, vs }) => {
    vs.forEach((v) => u_.searchParams.append(k, v));
  });
  return u_;
};

const setOnCloned = (k) => (s) => (u) => {
  const u_ = cloned(u);
  u_[k] = s;
  return u_;
};

export const setHashImpl = setOnCloned("hash");

export const setHostImpl = setOnCloned("hostname");

export const setHrefImpl = setOnCloned("href");

export const setPasswordImpl = setOnCloned("password");

export const setPathnameImpl = setOnCloned("pathname");

export const setPortImpl = setOnCloned("port");

export const setProtocolImpl = setOnCloned("protocol");

export const setSearchImpl = setOnCloned("search");

export const setUsernameImpl = setOnCloned("username");
