#!/usr/bin/env python3
"""Check native relocation captures independently, then compare semantic goldens."""

import argparse
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys

from macho_metadata import OracleError, read_object, require
from macho_arm64 import verify
from golden_reference import replay_reference


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def compare_golden(report, golden):
    for field in ('schema', 'format', 'object_sha256', 'semantic'):
        require(report[field] == golden[field], 'golden differs: ' + field)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--object', type=Path, required=True)
    parser.add_argument('--capture', type=Path, required=True)
    parser.add_argument('--readobj', default='llvm-readobj')
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument('--golden', type=Path, help='compare after checking actual relocated fields')
    mode.add_argument('--write-golden', type=Path, help='create a new oracle-checked reference; refuses replacement')
    parser.add_argument('--evidence-dir', type=Path, help='save LLVM output, commands and numeric field checks')
    args = parser.parse_args(argv)
    try:
        readobj = shutil.which(args.readobj)
        require(readobj is not None, 'missing llvm-readobj: ' + args.readobj)
        metadata, reader = read_object(args.object, readobj)
        capture = json.loads(args.capture.read_text())
        report = verify(metadata, capture, args.object.read_bytes())
        if args.golden:
            golden = json.loads(args.golden.read_text())
            replay_reference(metadata, golden, args.object.read_bytes())
            compare_golden(report, golden)
        if args.evidence_dir:
            require(not args.evidence_dir.exists(), 'evidence directory already exists')
        if args.write_golden:
            args.write_golden.parent.mkdir(parents=True, exist_ok=True)
            with args.write_golden.open('x') as file:
                json.dump(report, file, indent=2, sort_keys=True)
                file.write('\n')
        if args.evidence_dir:
            args.evidence_dir.mkdir(parents=True, exist_ok=False)
            inputs = {str(p.resolve()): digest(p) for p in
                      [args.object, args.capture, Path(readobj), Path(__file__),
                       Path(__file__).with_name('macho_metadata.py'), Path(__file__).with_name('macho_arm64.py'),
                       Path(__file__).with_name('golden_reference.py')]}
            (args.evidence_dir / 'llvm.stdout').write_text(reader['stdout'])
            (args.evidence_dir / 'llvm.stderr').write_text(reader['stderr'])
            (args.evidence_dir / 'command.json').write_text(json.dumps(dict(
                argv=sys.argv if argv is None else argv, reader_argv=reader['argv'],
                reader_exit=reader['exit'], reader_version=reader['version'],
                input_sha256=inputs), indent=2) + '\n')
            (args.evidence_dir / 'report.json').write_text(json.dumps(report, indent=2, sort_keys=True) + '\n')
        print(json.dumps(dict(status='pass', object_sha256=report['object_sha256'],
                              relocation_records=len(report['fields']), sections=len(metadata['sections']))))
        return 0
    except (OracleError, OSError, ValueError, KeyError, TypeError, subprocess.SubprocessError) as error:
        print('relocation check failed: ' + str(error), file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
