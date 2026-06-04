// run.mjs — Node.js launcher for a GHC-wasm reactor module.
//
// Invoke as: node run.mjs
// Assumes:   ./myapp.wasm exists, ./jsffi.mjs is post-link.mjs output for it.
//
// Empirically verified 2026-05-26 against stable-haskell/ghc 9.14 wasm cross.
import { readFile } from "node:fs/promises";
import { WASI } from "node:wasi";

const mod = await WebAssembly.compile(await readFile("./myapp.wasm"));
const jsffi = (await import("./jsffi.mjs")).default;

const wasi = new WASI({ version: "preview1", args: ["myapp.wasm"] });

// Knot-tying: __exports starts empty; jsffi closures capture it by
// reference; after instantiate we Object.assign instance.exports into it,
// so any later jsffi call can reach back into the wasm module.
const __exports = {};
const instance = await WebAssembly.instantiate(mod, {
  ghc_wasm_jsffi: jsffi(__exports),
  wasi_snapshot_preview1: wasi.wasiImport,
});
Object.assign(__exports, instance.exports);

// Reactor bring-up sequence — order matters:
//  1. wasi.initialize() runs WASI static constructors (_initialize export).
//  2. __ghc_wasm_jsffi_init() initializes GHC's JSFFI runtime + the RTS.
//     Without this, hs_start() throws "RTS is not initialised; call hs_init()".
//  3. hs_start() runs the Haskell entry point we foreign-exported.
wasi.initialize(instance);
instance.exports.__ghc_wasm_jsffi_init();
await instance.exports.hs_start();
