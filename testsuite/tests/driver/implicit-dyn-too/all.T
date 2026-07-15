# -dynamic-too is deprecated and a no-op; all .o files are now dynamic-capable.
# This test exercised implicit -dynamic-too recompilation which no longer exists.
test('implicit-dyn-too', [skip],  # -dynamic-too is deprecated
     makefile_test, [])
