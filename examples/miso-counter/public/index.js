// Browser launcher for the GHC-wasm reactor module (miso variant).
// Same shape as the hello template's index.js — the work happens in Main.hs.

import {
  WASI,
  OpenFile,
  File,
  ConsoleStdout,
} from "https://esm.sh/@bjorn3/browser_wasi_shim@0.4.2";

import ghc_wasm_jsffi from "./jsffi.mjs";

const args = ["myapp.wasm"];
const env  = ["GHCRTS=-H64m"];
const fds  = [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered((m) => console.log (`[hs stdout] ${m}`)),
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

// Reactor bring-up sequence (see hello/run.mjs for commentary).
wasi.initialize(instance);
instance.exports.__ghc_wasm_jsffi_init();
await instance.exports.hs_start();
