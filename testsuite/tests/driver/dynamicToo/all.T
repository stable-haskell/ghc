
# -dynamic-too is deprecated and a no-op; all .o files are now dynamic-capable.
# These tests exercised -dynamic-too functionality that no longer exists.

test('dynamicToo003',
     [extra_files(['A003.hs']),
      skip],  # -dynamic-too is deprecated
     makefile_test, [])

test('dynamicTooRecomp',
     [extra_files(['ARecomp.hs']),
      skip],  # -dynamic-too is deprecated
     makefile_test, [])

test('dynamicTooOnlyInterface',
     [extra_files(['ARecomp.hs']),
      skip],  # -dynamic-too is deprecated
     makefile_test, [])
