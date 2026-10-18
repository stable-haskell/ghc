"""Check missing and supplied ELF visibility macros in both ELF classes."""

import pathlib
import subprocess
import sys
import tempfile


rts = pathlib.Path(sys.argv[1]).resolve()
cc = sys.argv[2]
with tempfile.TemporaryDirectory() as directory:
    work = pathlib.Path(directory)
    # Only preprocessing matters here. Empty system headers let the test model
    # libcs with missing macros, independently of the build machine's elf.h.
    (work / "elf.h").write_text("")
    (work / "ghcplatform.h").write_text("")
    for bits in (32, 64):
        native = f"ELF{bits}_ST_VISIBILITY"
        for supplied in (None, native, "ELF_ST_VISIBILITY"):
            source = (
                "#undef __LP64__\n#undef _LP64\n"
                + ("#define __LP64__ 1\n" if bits == 64 else "")
                + (f"#define {supplied}(o) ((o)&7)\n" if supplied else "")
                + '#include "linker/ElfTypes.h"\n'
                + f"#if {native}(255) != {7 if supplied == native else 3}\n"
                + '#error "incorrect native visibility macro"\n#endif\n'
                + f"#if ELF_ST_VISIBILITY(255) != {7 if supplied else 3}\n"
                + '#error "incorrect generic visibility macro"\n#endif\n'
            )
            subprocess.run(
                [cc, "-E", "-Werror", "-DOBJFORMAT_ELF", "-I", str(work),
                 "-I", str(rts), "-x", "c", "-"],
                input=source, text=True, check=True, stdout=subprocess.DEVNULL,
            )
