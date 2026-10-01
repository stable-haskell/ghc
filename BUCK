# Root BUCK file: exports of root files that buck2 rules need. A file at
# the root belongs to no package without this file.
[
    export_file(name = f, src = f, visibility = ["PUBLIC"])
    for f in ["config.sub", "config.guess", "install-sh"]
]
