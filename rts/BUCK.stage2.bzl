# The stage-2 rules of the rts (see BUCK), once stage 2 is generated (see
# Note [Variant stubs] in cabal-install's Distribution.Client.Buck2.Generate).
load(":BUCK.stage2.cabal.bzl", "GENERATED", "VERSION", generated_targets_stage2 = "generated_targets")
load("//buck2:haskell.bzl", "haskell_library")
load("//buck2:cabal_overrides.bzl", "remove")

WAYS = ["nonthreaded-nodebug", "threaded-nodebug", "nonthreaded-debug", "threaded-debug"]

def stage2_rules():
    if not GENERATED:
        return

    haskell_library(
        name = "rts-constants-stage2",
        srcs = {},
        unit_id = "rts",
        package_name = "rts",
        version = VERSION,
        # the same headers as rts-stage2 (GHC compiles a C stub against Rts.h
        # when it links a program, with the include-dirs of the unit `rts`)
        include_dirs = ["//rts/cabal-buck2/autogen-stage2:rts-stage2-configure-include", "include"],
        deps = ["//rts-headers:rts-headers-stage2"],
        default_target_platform = "root//buck2/platforms:stage2",
    )

    generated_targets_stage2(overrides = dict(
        {
            way + "-stage2": {
                "deps": [remove(["//rts:rts-stage2"]), ":rts-constants-stage2"],
                # The rts-headers for GHC's Cmm preprocessing: the prelude
                # passes the headers of a dependency to the Haskell
                # preprocessor (-optP) and the C compiler (-optc) only.
                "compiler_flags": ["-Irts-headers/include"],
                # rts.cabal gives these files per-file options too
                # (`AutoApply_V32.cmm (-mavx2)`); upstream Cabal keeps only the
                # first of two identical `(-mavx2)` entries, so the generated
                # per_src_flags has them for Jumps_V*.cmm only.
                "per_src_flags": {
                    "AutoApply_V32.cmm": ["-mavx2"],
                    "AutoApply_V64.cmm": ["-mavx512f"],
                },
            }
            for way in WAYS
        },
        **{
            "rts-stage2": {
                "sublibraries": [":" + way + "-stage2" for way in WAYS],
            },
        }
    ))
