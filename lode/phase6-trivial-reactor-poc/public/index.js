// Browser launcher for a GHC-wasm reactor module.
//
// Pairs with `run.mjs` (Node.js variant). Same invocation pattern; only the
// WASI provider differs — browsers use browser_wasi_shim, Node uses
// the built-in node:wasi.
//
// Empirically verified 2026-05-26 (the Node variant); browser variant is
// the same pattern with browser_wasi_shim swapped in for node:wasi.
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

// Reactor bring-up sequence — order matters:
//  1. wasi.initialize() runs WASI static constructors (_initialize export).
//  2. __ghc_wasm_jsffi_init() initializes GHC's JSFFI runtime + the RTS.
//     Without this, hs_start() throws "RTS is not initialised; call hs_init()".
//  3. hs_start() runs the Haskell entry point we foreign-exported.
wasi.initialize(instance);
instance.exports.__ghc_wasm_jsffi_init();
await instance.exports.hs_start();
