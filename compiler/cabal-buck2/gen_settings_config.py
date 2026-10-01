#!/usr/bin/env python3
"""Generate GHC/Settings/Config.hs like compiler/Setup.hs does.

Usage: gen_settings_config.py --ghc GHC --unit-id UID
          (--ghc-internal-unit-id UID | --ghc-pkg GHC_PKG) -o OUT

compiler/Setup.hs (build-type Custom) writes this module at configure
time from `ghc --info` of the compiler that builds the library, the
library's own unit id, and the unit id of the ghc-internal it depends on.
cabal buck2 does not run Custom setups, so a buck2 genrule runs this
script instead. Without --ghc-internal-unit-id (stage 1), ghc-internal is
looked up in the compiler's package db with --ghc-pkg; a boot compiler
before 9.10 has none ("<unavailable>", as in Setup.hs).
"""
import argparse
import ast
import subprocess

ap = argparse.ArgumentParser()
ap.add_argument("--ghc", required=True)
ap.add_argument("--unit-id", required=True)
ap.add_argument("--ghc-internal-unit-id")
ap.add_argument("--ghc-pkg")
ap.add_argument("-o", required=True)
args = ap.parse_args()

info = dict(ast.literal_eval(subprocess.check_output([args.ghc, "--info"], text=True)))

# Setup.hs: cStage is hard-coded to 2, even for the stage-1 compiler.
settings = {
    "cBuildPlatformString": info["Host platform"],
    "cHostPlatformString": info["Target platform"],
    "cProjectName": info["Project name"],
    "cBooterVersion": info["Project version"],
    "cStage": "2",
}

if args.ghc_internal_unit_id is not None:
    ghc_internal_unit_id = args.ghc_internal_unit_id
else:
    r = subprocess.run([args.ghc_pkg, "field", "ghc-internal", "id"],
                       capture_output=True, text=True)
    if r.returncode == 0 and r.stdout.strip():
        ghc_internal_unit_id = r.stdout.split(":", 1)[1].strip()
    else:
        ghc_internal_unit_id = "<unavailable>"

def hs_str(s):
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'

lines = [
    "module GHC.Settings.Config",
    "  ( module GHC.Version",
    "  , cBuildPlatformString",
    "  , cHostPlatformString",
    "  , cProjectName",
    "  , cBooterVersion",
    "  , cStage",
    "  , cProjectUnitId",
    "  , cGhcInternalUnitId",
    "  ) where",
    "",
    "import GHC.Prelude.Basic",
    "",
    "import GHC.Version",
    "",
    "cBuildPlatformString :: String",
    "cBuildPlatformString = " + hs_str(settings["cBuildPlatformString"]),
    "",
    "cHostPlatformString :: String",
    "cHostPlatformString = " + hs_str(settings["cHostPlatformString"]),
    "",
    "cProjectName          :: String",
    "cProjectName          = " + hs_str(settings["cProjectName"]),
    "",
    "cBooterVersion        :: String",
    "cBooterVersion        = " + hs_str(settings["cBooterVersion"]),
    "",
    "cStage                :: String",
    "cStage                = show (" + settings["cStage"] + " :: Int)",
    "",
    "cProjectUnitId :: String",
    "cProjectUnitId = " + hs_str(args.unit_id),
    "",
    "cGhcInternalUnitId :: String",
    "cGhcInternalUnitId = " + hs_str(ghc_internal_unit_id),
]

with open(args.o, "w") as f:
    f.write("\n".join(lines) + "\n")
