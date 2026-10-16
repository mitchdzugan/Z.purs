import git from "git-client";
import path from "node:path";

export function js_newClient($workTree) {
  const $gitDir = path.join($workTree, ".git");
  const gitOpts = { $gitDir, $workTree };
  return {
    baseDir: $workTree,
    pull: () => git.pull(gitOpts),
    push: () => git.push(gitOpts),
    status: (porcelain) => () => git.status({ porcelain }, gitOpts),
    revParse: (short) => (hash) => () => git.revParse({ short }, hash, gitOpts),
    add: (all) => () => git.add({ all }, gitOpts),
    commit: (message) => () => git.commit({ message }, gitOpts),
    checkout: (paths) => () => git.checkout(...paths, gitOpts),
  };
}

export const js_clientBaseDir = (c) => c.baseDir;
export const js_pull = (c) => c.pull;
export const js_push = (c) => c.push;
export const js_status = (c) => c.status;
export const js_revParse = (c) => c.revParse;
export const js_add = (c) => c.add;
export const js_commit = (c) => c.commit;
export const js_checkout = (c) => c.checkout;
