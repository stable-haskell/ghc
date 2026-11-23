// Browser launcher for a GHC-wasm reactor module.
// Validated against haskell-wasm/ghc-wasm-miso-examples/frontend/index.js
// and the GHC user's guide §15 knot-tying pattern.
//
// FIXME: pin version to match what our RTS expects. Per project memory,
// commit e0837350f14 bumped browser_wasi_shim to 0.4.2 on the GHC side.
// Verify the launcher version matches the WASI imports the wasm module declares.
import {
  WASI,
  OpenFile,
  File,
  ConsoleStdout,
} from "https://esm.sh/@bjorn3/browser_wasi_shim@0.4.2";

// post-link.mjs writes a module whose *default export* is a function
// that, given the (eventually-populated) exports object, returns the
// imports object for the `ghc_wasm_jsffi` namespace. We pre-bind it
// against an empty record and assign exports after instantiation -
// this is the standard knot-tying trick from the GHC user's guide.
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

const args = [];                          // argv[0] is unused for reactors
const env  = ["GHCRTS=-H64m"];            // small initial heap; tune as needed
const fds  = [
  new OpenFile(new File([])),                                            // 0: stdin
  ConsoleStdout.lineBuffered((m) => console.log (`[hs stdout] ${m}`)),   // 1
  ConsoleStdout.lineBuffered((m) => console.warn(`[hs stderr] ${m}`)),   // 2
];
const wasi = new WASI(args, env, fds, { debug: false });

const instance_exports = {};
const { instance } = await WebAssembly.instantiateStreaming(
  fetch("./myapp.wasm"),
  {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi:         ghc_wasm_jsffi(instance_exports),
  },
);
// Tie the knot: now jsffi callbacks can reach back into the wasm module.
Object.assign(instance_exports, instance.exports);

// Reactor module bring-up sequence — order matters:
//  1. wasi.initialize() runs WASI static constructors (_initialize export).
//  2. __ghc_wasm_jsffi_init() initializes GHC's JSFFI runtime + the RTS.
//     Without this, hs_start() throws "RTS is not initialised; call hs_init()".
//     Empirically verified 2026-05-26.
//  3. hs_start() runs the Haskell entry point (whatever you `foreign export`ed).
wasi.initialize(instance);
instance.exports.__ghc_wasm_jsffi_init();
await instance.exports.hs_start();
