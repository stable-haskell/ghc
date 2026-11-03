#!/usr/bin/env python3
"""Generate GHC/Platform/Host.hs the way libraries/ghc-boot/configure does.

Usage: gen_platform_host.py <ghc> <output-file>

The configure script writes this module from Cabal's --host= triple.
`cabal buck2` does not run that script, so a buck2 genrule runs this one
with the boot compiler's target platform. Constructors are emitted
directly; the library does not depend on Cabal-syntax.
"""
import subprocess
import sys

ghc, out = sys.argv[1:3]

triple = subprocess.check_output([ghc, "--print-target-platform"], text=True).strip()
arch, _vendor, os_ = triple.split("-", 2)

ARCHS = {
    "i386": "ArchX86",
    "i686": "ArchX86",
    "x86_64": "ArchX86_64",
    "amd64": "ArchX86_64",
    "ppc": "ArchPPC",
    "powerpc": "ArchPPC",
    "ppc64": "ArchPPC_64 ELF_V1",
    "powerpc64": "ArchPPC_64 ELF_V1",
    "ppc64le": "ArchPPC_64 ELF_V2",
    "powerpc64le": "ArchPPC_64 ELF_V2",
    "arm": "ArchARM ARMv7 [] SOFT",
    "armv5": "ArchARM ARMv7 [] SOFT",
    "armv6": "ArchARM ARMv7 [] SOFT",
    "armv7": "ArchARM ARMv7 [] SOFT",
    "armel": "ArchARM ARMv7 [] SOFT",
    "armeb": "ArchARM ARMv7 [] SOFT",
    "aarch64": "ArchAArch64",
    "arm64": "ArchAArch64",
    "alpha": "ArchAlpha",
    "riscv64": "ArchRISCV64",
    "loongarch64": "ArchLoongArch64",
    "javascript": "ArchJavaScript",
    "wasm32": "ArchWasm32",
}

# Android used to map to OSUnknown in Setup.hs. Strip a libc suffix only
# after that special case, so linux-android does not become Linux.
if os_.startswith("linux-android") or os_.startswith("android"):
    os_hs = "OSUnknown"
elif os_.startswith("dragonfly"):
    # Cabal's triple is unknown-dragonflybsd. The table key is the GNU name.
    os_hs = "OSDragonFly"
else:
    os_base = os_.split("-")[0]
    os_hs = {
        "linux": "OSLinux",
        "darwin": "OSDarwin",
        "mingw32": "OSMinGW32",
        "windows": "OSMinGW32",
        "freebsd": "OSFreeBSD",
        "openbsd": "OSOpenBSD",
        "netbsd": "OSNetBSD",
        "dragonfly": "OSDragonFly",
        "solaris": "OSSolaris2",
        "solaris2": "OSSolaris2",
        "aix": "OSAIX",
        "gnu": "OSHurd",
        "hurd": "OSHurd",
        "ghcjs": "OSGhcjs",
        "wasi": "OSWasi",
        "haiku": "OSHaiku",
    }.get(os_base, "OSUnknown")

arch_hs = ARCHS.get(arch, "ArchUnknown")

lines = [
    "module GHC.Platform.Host where",
    "",
    "import GHC.Platform.ArchOS",
    "",
    "hostPlatformArch :: Arch",
    "hostPlatformArch = " + arch_hs,
    "",
    "hostPlatformOS   :: OS",
    "hostPlatformOS   = " + os_hs,
    "",
    "hostPlatformArchOS :: ArchOS",
    "hostPlatformArchOS = ArchOS hostPlatformArch hostPlatformOS",
    "",
]

with open(out, "w") as f:
    f.write("\n".join(lines))
