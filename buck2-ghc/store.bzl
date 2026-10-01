# A GHC installation ("store") built with buck2, and wrappers that run a
# compiler with it: the stage-1 compiler with an empty package db (the
# stage-2 toolchain, see .buckconfig [haskell_stage2]) and the stage-2
# compiler with every stage-2 library registered (the result of the
# build). See buck2-ghc/BUCK.
load("@third-party-haskell//:tools.bzl", "GHC_VERSION")

def ghc_store(name, unlit, package_db = None, **kwargs):
    """lib/ is the libdir: `settings` from ghc-toolchain-bin for the boot
    compiler's host platform, the four per-target dials the Makefile adds
    for a static, non-profiled stage 2, and the package db (`package_db`,
    a haskell_package_db target, or an empty one). bin/ holds unlit,
    which GHC runs from <libdir>/../bin."""
    if package_db:
        db_cmd = "cp -r $(location " + package_db + ")/. $OUT/lib/package.conf.d"
    else:
        db_cmd = "$(exe //utils/ghc-pkg:ghc-pkg) init $OUT/lib/package.conf.d"
    native.genrule(
        name = name,
        # ghc-toolchain-bin normalises the triple with `sh config.sub` in its
        # working directory, which for a genrule is the srcs directory.
        srcs = ["//:config.sub"],
        out = "store",
        cmd = " && ".join([
            "mkdir -p $OUT/lib $OUT/bin",
            "$(exe //utils/ghc-toolchain/exe:ghc-toolchain-bin) --disable-ld-override --triple `ghc-" + GHC_VERSION + " --print-host-platform` --cc cc --cxx c++ --cc-link-opt '' --output-settings -o $OUT/lib/settings",
            "sed -i -e 's/\\]$/,(\"target is dynamic\",\"NO\"),(\"target ships dynamic libraries\",\"NO\"),(\"target is profiled\",\"NO\"),(\"target ships profiling libraries\",\"NO\")]/' $OUT/lib/settings",
            db_cmd,
            # a link, not a copy: unlit finds its shared libraries next to
            # its own output
            "ln -s `realpath $(location " + unlit + ")` $OUT/bin/unlit",
        ]),
        **kwargs
    )

def ghc_wrapper(name, ghc, store, **kwargs):
    """`ghc` with -B<libdir>. Absolute paths are baked in (realpath at
    generation time): cabal and GHC both run the wrapper from other
    directories than the project root."""
    native.genrule(
        name = name,
        out = name,
        cmd = "printf '#!/bin/sh\\nexec \"%s\" -B\"%s/lib\" \"$@\"\\n' `realpath $(location " + ghc + ")` `realpath $(location " + store + ")` > $OUT && chmod +x $OUT",
        executable = True,
        visibility = ["PUBLIC"],
        **kwargs
    )

def ghc_pkg_wrapper(name, ghc_pkg, store, **kwargs):
    """`ghc-pkg` with the store's package db as global db."""
    native.genrule(
        name = name,
        out = name,
        cmd = "printf '#!/bin/sh\\nexec \"%s\" --global-package-db \"%s/lib/package.conf.d\" \"$@\"\\n' `realpath $(location " + ghc_pkg + ")` `realpath $(location " + store + ")` > $OUT && chmod +x $OUT",
        executable = True,
        visibility = ["PUBLIC"],
        **kwargs
    )
