# -dynamic-too is deprecated and a no-op; all .o files are now dynamic-capable.
test('recomp-boot-dyn-too', [ skip ],  # -dynamic-too is deprecated
     makefile_test, [])
