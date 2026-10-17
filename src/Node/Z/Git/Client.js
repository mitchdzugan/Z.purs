import git from "git-client";
import path from "node:path";

export function js_newClient($workTree) {
  const $gitDir = path.join($workTree, ".git");
  const gitOpts = { $gitDir, $workTree };
  return {
    baseDir: $workTree,
    gitFn: (cmd) => (opts) => (args) => () => git[cmd](opts, ...args, gitOpts),
  };
}
export const js_clientBaseDir = (c) => c.baseDir;
export const js_gitFn = (c) => c.gitFn;
