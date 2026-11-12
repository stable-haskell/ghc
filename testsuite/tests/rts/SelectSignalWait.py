"""Exercise the production wait helper without the rest of the RTS scheduler."""
from pathlib import Path
import re
import sys

source = Path(sys.argv[1]).read_text()
helpers = re.findall(r"^static int\nselectWithUserSignals\(.*?^}\n", source,
                     re.MULTILINE | re.DOTALL)
if len(helpers) != 1:
    raise SystemExit("expected one production selectWithUserSignals definition")
Path("SelectSignalWait.inc").write_text(helpers[0])
