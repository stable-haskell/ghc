"""Check portable visibility helpers and preserve supplied SDK macros."""

import pathlib
import subprocess
import sys
import tempfile


source_root = pathlib.Path(sys.argv[1]).resolve()
public_root = source_root.parent / "include"
cc = sys.argv[2]
with tempfile.TemporaryDirectory() as directory:
    work = pathlib.Path(directory)
    # Only preprocessing matters here. Empty platform configuration isolates
    # the portable helpers from the host SDK; it does not model SDK admission.
    (work / "ll_platform.h").write_text("")
    for bits in (32, 64):
        native = f"ELF{bits}_ST_VISIBILITY"
        target = "LL_ARCH_I386" if bits == 32 else "LL_ARCH_X86_64"
        for supplied in ((), (native,), ("ELF_ST_VISIBILITY",),
                         ("ELF32_ST_VISIBILITY", "ELF64_ST_VISIBILITY",
                          "ELF_ST_VISIBILITY")):
            source = (
                f"#define LL_TARGET_ARCH {target}\n"
                + "".join(f"#define {name}(o) ((o)&7)\n" for name in supplied)
                + '#include "linker/ElfTypes.h"\n'
                + f"#if LL_ELF_CLASS != LL_ELF_ELFCLASS{bits}\n"
                + '#error "incorrect target ELF class"\n#endif\n'
            )
            for value in (*range(8), 252, 255):
                for name in ("LL_ELF32_ST_VISIBILITY", "LL_ELF64_ST_VISIBILITY",
                             "LL_ELF_ST_VISIBILITY"):
                    source += (
                        f"#if {name}({value}) != {value & 3}\n"
                        + '#error "incorrect portable visibility macro"\n#endif\n'
                    )
                for name in supplied:
                    source += (
                        f"#if {name}({value}) != {value & 7}\n"
                        + '#error "supplied SDK macro changed"\n#endif\n'
                    )
            subprocess.run(
                [cc, "-E", "-Werror", "-DOBJFORMAT_ELF", "-I", str(work),
                 "-I", str(source_root), "-I", str(public_root), "-x", "c", "-"],
                input=source, text=True, check=True, stdout=subprocess.DEVNULL,
            )
