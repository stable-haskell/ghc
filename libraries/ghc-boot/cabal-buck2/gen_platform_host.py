#!/usr/bin/env python3
"""Generate GHC/Platform/Host.hs like libraries/ghc-boot/Setup.hs does.

Usage: gen_platform_host.py <ghc> <output-file>

Setup.hs (build-type Custom) writes this module at configure time from
Cabal's host platform. `cabal buck2` does not run Custom setups, so a
buck2 genrule runs this script instead, with the target platform of the
boot compiler (the platform the new compiler runs on).
"""
import subprocess
import sys

ghc, out = sys.argv[1:3]

triple = subprocess.check_output([ghc, "--print-target-platform"], text=True).strip()
arch, _vendor, os_ = triple.split("-", 2)

# GHC arch/OS names -> Distribution.System constructor names (Cabal)
ARCHS = {
    "x86_64": "X86_64", "i386": "I386", "aarch64": "AArch64", "arm": "Arm",
    "powerpc": "PPC", "powerpc64": "PPC64", "powerpc64le": "PPC64LE",
    "riscv64": "RISCV64", "loongarch64": "LoongArch64",
    "javascript": "JavaScript", "wasm32": "Wasm32",
}
OSES = {
    "linux": "Linux", "darwin": "OSX", "mingw32": "Windows", "freebsd": "FreeBSD",
    "openbsd": "OpenBSD", "netbsd": "NetBSD", "dragonfly": "DragonFly",
    "solaris2": "Solaris", "aix": "AIX", "gnu": "Hurd", "ghcjs": "Ghcjs",
    "wasi": "Wasi", "haiku": "Haiku",
}
# Strip a libc/ABI suffix such as "linux-gnu" or "linux-musl".
os_ = os_.split("-")[0]
cabal_arch = ARCHS.get(arch, '(OtherArch "%s")' % arch)
cabal_os = OSES.get(os_, '(OtherOS "%s")' % os_)

lines = [
    "module GHC.Platform.Host where",
    "",
    "import GHC.Platform.ArchOS",
    "import Distribution.System hiding (Arch, OS)",
    "",
    "hostPlatformArch :: Arch",
    "hostPlatformArch = toArch " + cabal_arch,
    "",
    "hostPlatformOS   :: OS",
    "hostPlatformOS   = toOS " + cabal_os,
    "",
    "hostPlatformArchOS :: ArchOS",
    "hostPlatformArchOS = ArchOS hostPlatformArch hostPlatformOS",
    "",
    "toArch I386 = ArchX86",
    "toArch X86_64 = ArchX86_64",
    "toArch PPC = ArchPPC",
    "toArch PPC64 = ArchPPC_64 ELF_V1",
    "toArch PPC64LE = ArchPPC_64 ELF_V2",
    "toArch Sparc = ArchUnknown -- ?",
    "toArch Sparc64 = ArchUnknown -- ?",
    "toArch Arm = ArchARM ARMv7 [] SOFT -- ?",
    "toArch AArch64 = ArchAArch64",
    "toArch Mips = ArchUnknown -- ?",
    "toArch SH = ArchUnknown -- ?",
    "toArch IA64 = ArchUnknown -- ?",
    "toArch S390 = ArchUnknown -- ?",
    "toArch S390X = ArchUnknown -- ?",
    "toArch Alpha = ArchAlpha",
    "toArch Hppa = ArchUnknown -- ?",
    "toArch Rs6000 = ArchUnknown -- ?",
    "toArch M68k = ArchUnknown -- ?",
    "toArch Vax = ArchUnknown -- ?",
    "toArch RISCV64 = ArchRISCV64",
    "toArch LoongArch64 = ArchLoongArch64",
    "toArch JavaScript = ArchJavaScript",
    "toArch Wasm32 = ArchWasm32",
    "toArch (OtherArch _) = ArchUnknown",
    "",
    "toOS Linux = OSLinux",
    "toOS Windows = OSMinGW32",
    "toOS OSX = OSDarwin",
    "toOS FreeBSD = OSFreeBSD",
    "toOS OpenBSD = OSOpenBSD",
    "toOS NetBSD = OSNetBSD",
    "toOS DragonFly = OSDragonFly",
    "toOS Solaris = OSSolaris2",
    "toOS AIX = OSAIX",
    "toOS HPUX = OSUnknown -- ?",
    "toOS IRIX = OSUnknown -- ?",
    "toOS HaLVM = OSUnknown -- ?",
    "toOS Hurd = OSHurd",
    "toOS IOS = OSUnknown -- ?",
    "toOS Android = OSUnknown -- ?",
    "toOS Ghcjs = OSGhcjs",
    "toOS Wasi = OSWasi",
    "toOS Haiku = OSHaiku",
    "toOS (OtherOS _) = OSUnknown",
]

with open(out, "w") as f:
    f.write("\n".join(lines) + "\n")
