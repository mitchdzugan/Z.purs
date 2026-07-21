import { spawn } from "node:child_process";

const bProc = spawn("npx", ["spago", "build", "--json-errors"]);
const fProc = spawn("npx", ["ps-suggest", "--apply"]);

bProc.stderr.pipe(fProc.stdin);
bProc.stdout.pipe(fProc.stdin);

bProc.stdout.on("data", (d) => console.log("build:i", d.toString().trim()));
bProc.stderr.on("data", (d) => console.error("build:e", d.toString().trim()));

fProc.stdout.on("data", (d) => console.log("fix:i", d.toString().trim()));
fProc.stderr.on("data", (d) => console.error("fix:e", d.toString().trim()));
