import {
  WASI,
  OpenFile,
  File,
  ConsoleStdout,
} from "https://esm.sh/@bjorn3/browser_wasi_shim@0.4.2";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

const args = [];
const env  = ["GHCRTS=-H64m"];
const fds  = [
  new OpenFile(new File([])),
  ConsoleStdout.lineBuffered((m) => console.log (`[hs stdout] ${m}`)),
  ConsoleStdout.lineBuffered((m) => console.warn(`[hs stderr] ${m}`)),
];
const wasi = new WASI(args, env, fds, { debug: false });

const instance_exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("./app.wasm"), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi:         ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);
instance.exports.__ghc_wasm_jsffi_init();
await instance.exports.hs_start(globalThis.example);
