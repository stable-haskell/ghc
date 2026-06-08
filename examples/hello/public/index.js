// Browser launcher for the GHC-wasm reactor module.
// Mirrors run.mjs, but uses browser_wasi_shim instead of node:wasi.

import {
  WASI,
  OpenFile,
  File,
  ConsoleStdout,
} from "https://esm.sh/@bjorn3/browser_wasi_shim@0.4.2";

import ghc_wasm_jsffi from "./jsffi.mjs";

const out = document.getElementById("out");
const lines = [];
const log = (m) => {
  console.log(`[hs stdout] ${m}`);
  lines.push(m);
  if (out) out.textContent = lines.join("\n");
};

const args = ["myapp.wasm"];
const env  = ["GHCRTS=-H64m"];
const fds  = [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered(log),
  ConsoleStdout.lineBuffered((m) => console.warn(`[hs stderr] ${m}`)),
];
const wasi = new WASI(args, env, fds, { debug: false });

const __exports = {};
const { instance } = await WebAssembly.instantiateStreaming(
  fetch("./myapp.wasm"),
  {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi:         ghc_wasm_jsffi(__exports),
  },
);
Object.assign(__exports, instance.exports);

// Reactor bring-up sequence — order matters (see run.mjs comments).
wasi.initialize(instance);
instance.exports.__ghc_wasm_jsffi_init();
await instance.exports.hs_start();
