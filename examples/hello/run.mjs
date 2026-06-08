// run.mjs — Node.js launcher for a GHC-wasm reactor module.
//
// Invoke as: node run.mjs   (or: make run-node)
// Assumes:   ./myapp.wasm exists, ./jsffi.mjs is post-link.mjs output for it.
//
// Reactor bring-up sequence — order matters:
//   1. wasi.initialize()           — runs WASI static constructors (_initialize)
//   2. __ghc_wasm_jsffi_init()     — initializes GHC's JSFFI runtime + the RTS
//      (without this you get: "RTS is not initialised; call hs_init() first")
//   3. hs_start()                  — your foreign-exported entry point

import { readFile } from "node:fs/promises";
import { WASI } from "node:wasi";

const mod   = await WebAssembly.compile(await readFile("./myapp.wasm"));
const jsffi = (await import("./jsffi.mjs")).default;
const wasi  = new WASI({ version: "preview1", args: ["myapp.wasm"] });

// Knot-tying: __exports starts empty; jsffi closures capture it by reference.
// After WebAssembly.instantiate, we Object.assign instance.exports into it so
// any later jsffi call can reach back into the wasm module.
const __exports = {};
const instance = await WebAssembly.instantiate(mod, {
  ghc_wasm_jsffi:           jsffi(__exports),
  wasi_snapshot_preview1:   wasi.wasiImport,
});
Object.assign(__exports, instance.exports);

wasi.initialize(instance);
instance.exports.__ghc_wasm_jsffi_init();
await instance.exports.hs_start();
