#!/usr/bin/env -S awk -f

BEGIN {
  IGNORECASE = 1
}

BEGINFILE {
  if (version == "") {
    print "usage: update_versions.awk version=x.y.w.z [FILES..]" > "/dev/stderr"
    exit 1
  }
}

match($0, /^version\s*:\s*/, arr) {
  print arr[0] version
  next
}

{
  print
}
