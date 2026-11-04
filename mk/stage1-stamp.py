#!/usr/bin/env python3
"""Copy stdin to stdout and append "epoch<TAB>line" to a log.

Python 3.6, which is /usr/bin/python3 on Rocky Linux 8. The chart
renderer in stage1-build-graph.py needs 3.7. This stamper is the
process on the other side of the stage1 cabal pipe, so it has to
start on that interpreter. A syntax error here closes the pipe and
the compiler build stops.

    mk/stage1-stamp.py LOG
"""

from __future__ import print_function

import sys
import time


def main(argv):
    if len(argv) != 1:
        sys.stderr.write("usage: stage1-stamp.py LOG\n")
        return 2
    with open(argv[0], "w") as log:
        for line in sys.stdin:
            sys.stdout.write(line)
            sys.stdout.flush()
            log.write("%.3f\t%s" % (time.time(), line))
            log.flush()
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
