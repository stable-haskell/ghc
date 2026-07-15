# -dynamic-too is deprecated and a no-op; all .o files are now dynamic-capable.
# These tests exercised -dynamic-too functionality that no longer exists.

opts = [ extra_files(['A.hs', 'B1.hs', 'B2.hs', 'C.hs']),
      skip ]  # -dynamic-too is deprecated

test('dynamicToo001', [opts], makefile_test, [])
test('dynamicToo001MakeA', [opts], makefile_test, [])
test('dynamicToo001MakeB', [opts], makefile_test, [])
test('T25837', [opts], makefile_test, [])
