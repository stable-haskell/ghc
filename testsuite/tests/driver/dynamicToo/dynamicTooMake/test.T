# -dynamic-too is deprecated and a no-op; all .o files are now dynamic-capable.
test('dynamicTooMake',
     [extra_files(['p', 'q', 'Setup.hs']),
      skip],  # -dynamic-too is deprecated
     makefile_test, [])
