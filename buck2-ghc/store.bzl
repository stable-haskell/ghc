# A GHC installation ("store") built with buck2, and wrappers that run a
# compiler with it: the stage-1 compiler with an empty package db (the
# stage-2 toolchain, see .buckconfig [haskell_stage2]) and the stage-2
# compiler with every stage-2 library registered (the result of the
# build). See buck2-ghc/BUCK.
load("@third-party-haskell//:tools.bzl", "GHC_VERSION")
load("//ghc:BUCK.stage2.cabal.bzl", STAGE2_GENERATED = "GENERATED")
load("//buck2:package_db.bzl", "haskell_package_db")

def ghc_store(name, ghc, ghc_pkg, tools, package_db = None, hsc2hs = None, hsc2hs_data = None, **kwargs):
    """lib/ is the libdir: `settings` from ghc-toolchain-bin for the boot
    compiler's host platform, the four per-target dials the Makefile adds
    for a static, non-profiled stage 2, and the package db (`package_db`,
    a haskell_package_db target, or an empty one). bin/ holds `ghc` (the
    compiler target `ghc` with -B<libdir>), `ghc-pkg` (`ghc_pkg` with the
    libdir's db as global db) and the programs of `tools`, {name: target}:
    unlit at least, which GHC runs from <libdir>/../bin. Programs are
    links to the build outputs (realpath, so that they find their shared
    libraries); the scripts bake the store's own absolute path in.
    `hsc2hs` (the executable) gets a script that passes its template,
    lib/template-hsc.h, copied from `hsc2hs_data` (the package's
    data-files target): its Paths module does not find the data
    directory in a buck2 build."""
    if package_db:
        db_cmd = "cp -r $(location " + package_db + ")/. $OUT/lib/package.conf.d"
    else:
        db_cmd = "$(exe //utils/ghc-pkg:ghc-pkg) init $OUT/lib/package.conf.d"
    hsc2hs_cmds = [
        "cp $(location " + hsc2hs_data + ")/template-hsc.h $OUT/lib/template-hsc.h",
        "printf '#!/bin/sh\\nexec \"%s\" --template \"%s/lib/template-hsc.h\" \"$@\"\\n' `realpath $(location " + hsc2hs + ")` \"$STORE\" > $OUT/bin/hsc2hs",
        "chmod +x $OUT/bin/hsc2hs",
    ] if hsc2hs else []
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
            "STORE=`realpath $OUT`",
            "printf '#!/bin/sh\\nexec \"%s\" -B\"%s/lib\" \"$@\"\\n' `realpath $(location " + ghc + ")` \"$STORE\" > $OUT/bin/ghc",
            "printf '#!/bin/sh\\nexec \"%s\" --global-package-db \"%s/lib/package.conf.d\" \"$@\"\\n' `realpath $(location " + ghc_pkg + ")` \"$STORE\" > $OUT/bin/ghc-pkg",
            "chmod +x $OUT/bin/ghc $OUT/bin/ghc-pkg",
        ] + hsc2hs_cmds + [
            "ln -s `realpath $(location " + target + ")` $OUT/bin/" + tool
            for tool, target in tools.items()
        ]),
        **kwargs
    )

def store_program(name, store, program, **kwargs):
    """A program of `store`'s bin/ as a target of its own (a script that
    runs it), e.g. for the toolchain or `buck2 run`."""
    native.genrule(
        name = name,
        out = name,
        cmd = "printf '#!/bin/sh\\nexec \"%s/bin/" + program + "\" \"$@\"\\n' `realpath $(location " + store + ")` > $OUT && chmod +x $OUT",
        executable = True,
        visibility = ["PUBLIC"],
        **kwargs
    )

STAGE2 = "root//buck2/platforms:stage2"

def stage2_installation(libraries):
    """The stage-2 installation `stage2-libdir` (bin/ghc, bin/ghc-pkg and
    the other programs of the Makefile's STAGE2_EXECUTABLES; hsc2hs and
    hpc through the aliases of //dist-newstyle/src, see Note [Aliases for
    unpacked packages] in cabal-install's Distribution.Client.Buck2.Generate),
    with `libraries` registered, and the programs `ghc-stage2` and
    `ghc-pkg-stage2`. Nothing until stage 2 is generated. These targets
    depend on stage-2 targets, so they are configured for the stage-2
    platform themselves."""
    if not STAGE2_GENERATED:
        return
    haskell_package_db(
        name = "stage2-package-db",
        deps = libraries,
        default_target_platform = STAGE2,
    )
    ghc_store(
        name = "stage2-libdir",
        ghc = "//ghc:ghc-stage2",
        ghc_pkg = "//utils/ghc-pkg:ghc-pkg-stage2",
        tools = {
            "ghc-iserv": "//utils/ghc-iserv:ghc-iserv-stage2",
            "haddock": "//utils/haddock:haddock-stage2",
            "hp2ps": "//utils/hp2ps:hp2ps-stage2",
            "hpc": "//dist-newstyle/src:hpc-bin/hpc-stage2",
            "runghc": "//utils/runghc:runghc-stage2",
            "unlit": "//utils/unlit:unlit-stage2",
        },
        hsc2hs = "//dist-newstyle/src:hsc2hs-stage2",
        hsc2hs_data = "//dist-newstyle/src:hsc2hs-data-stage2",
        package_db = ":stage2-package-db",
        default_target_platform = STAGE2,
    )
    store_program(name = "ghc-stage2", store = ":stage2-libdir", program = "ghc", default_target_platform = STAGE2)
    store_program(name = "ghc-pkg-stage2", store = ":stage2-libdir", program = "ghc-pkg", default_target_platform = STAGE2)
