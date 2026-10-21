#!/usr/bin/env python3
"""Exercise distribution template copying without building a compiler."""

import argparse
import importlib.util
import json
from pathlib import Path
import shlex
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]
TEMPLATES = ("HappyTemplate.hs", "GLR_Base.hs", "GLR_Lib.hs")


class HappyTemplates(unittest.TestCase):
    make_command = ["make"]

    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="Happy templates ")
        self.addCleanup(self.temporary.cleanup)
        self.build = Path(self.temporary.name) / "build with spaces"
        self.store = self.build / "stage2/store/build/bootstrap-target"
        self.plan = self.build / "stage2/cache/plan.json"
        self.plan.parent.mkdir(parents=True)
        self.destination = self.build / "dist/share/happy-lib/data"
        self.python = Path(self.temporary.name) / "python with spaces"
        self.python.symlink_to(sys.executable)
        self.unit = {"type": "configured", "stage": "build", "id": "build:happy-lib-2.1.5-selected",
                     "pkg-name": "happy-lib", "component-name": "lib"}

    def write_data(self, unit_id, marker):
        data = self.store / "lib" / unit_id / "share"
        data.mkdir(parents=True)
        for name in TEMPLATES:
            (data / name).write_text(marker + name)
        return data

    def copy(self, units):
        self.plan.write_text(json.dumps({"install-plan": units}))
        rule = "copy-happy-test:\n\t@$(COPY_HAPPY_TEMPLATES)\n"
        return subprocess.run(
            [*self.make_command, "--no-print-directory", "-f", "Makefile", "-f", "-",
             "copy-happy-test", "BUILD_DIR=" + str(self.build), "BOOTSTRAP_TARGET=bootstrap-target",
             "HOST_PLATFORM=x86_64-unknown-linux", "STAGE2_PLATFORM=x86_64-unknown-linux-gnu",
             "GHC0=true", "PYTHON=" + shlex.quote(str(self.python))],
            input=rule, cwd=ROOT, universal_newlines=True,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=30)

    def test_selected_installed_data_without_source_cache(self):
        for state in ("configured", "installed"):
            with self.subTest(state=state):
                self.unit["type"] = state
                selected = self.store / "lib/happy-lib-2.1.5-selected/share"
                if not selected.exists():
                    self.write_data("happy-lib-2.1.5-selected", "selected ")
                    self.write_data("happy-lib-2.1.5-stale", "stale ")
                host = dict(self.unit, stage="host", id="host:happy-lib-2.1.5-wrong")
                sublib = dict(self.unit, **{"component-name": "lib:backend-glr", "id": "build:wrong-sublib"})
                result = self.copy([host, sublib, self.unit])
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertFalse((self.build / "stage2/src").exists())
                for name in TEMPLATES:
                    self.assertEqual((self.destination / name).read_bytes(), (selected / name).read_bytes())
                shutil.rmtree(self.destination)

    def test_missing_or_ambiguous_unit_does_not_copy_stale_data(self):
        self.write_data("happy-lib-2.1.5-stale", "stale ")
        for units in ([], [self.unit, dict(self.unit, id="build:another")]):
            with self.subTest(units=units):
                result = self.copy(units)
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("exactly one Build-stage", result.stderr)
                self.assertFalse(self.destination.exists())

    def test_inconsistent_stage_and_incomplete_data_fail(self):
        data = self.write_data("happy-lib-2.1.5-selected", "selected ")
        result = self.copy([dict(self.unit, id="host:happy-lib-2.1.5-selected")])
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("inconsistent unit identity", result.stderr)
        self.assertFalse(self.destination.exists())
        (data / "GLR_Lib.hs").unlink()
        result = self.copy([self.unit])
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("missing installed Happy template", result.stderr)
        self.assertFalse(self.destination.exists())

    def test_copy_with_pre_python38_api(self):
        data = self.write_data("happy-lib-2.1.5-selected", "selected ")
        self.plan.write_text(json.dumps({"install-plan": [self.unit]}))
        spec = importlib.util.spec_from_file_location("copy_happy_templates", ROOT / "mk/copy-happy-templates.py")
        helper = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(helper)
        original_copytree = shutil.copytree

        # Older hosts support copy_function but reject dirs_exist_ok.
        def old_copytree(source, destination, copy_function):
            return original_copytree(source, destination, copy_function=copy_function)

        with mock.patch.object(helper.shutil, "copytree", old_copytree):
            helper.copy_templates(self.plan, self.store, self.destination)
        for name in TEMPLATES:
            self.assertEqual((self.destination / name).read_bytes(), (data / name).read_bytes())


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--make", default="make", help="Make command and arguments (default: make)")
    args, remaining = parser.parse_known_args()
    HappyTemplates.make_command = shlex.split(args.make)
    if not HappyTemplates.make_command:
        parser.error("--make must name a command")
    unittest.main(argv=[sys.argv[0], *remaining])
