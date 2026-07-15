TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation test for -fno-code -fwrite-interface with TemplateHaskell.

checkExists = [ -f $1 ] || echo $1 missing

clean:
	rm -f *.o
	rm -f *.hi

T20348: clean
	# First run: should produce .hi and .o files.
	echo 'first run'
	'$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fwrite-interface B.hs
	$(call checkExists,A.hi)
	$(call checkExists,A.o)

	# Second run: should not recompile.
	echo 'second run'
	'$(TEST_HC)' $(TEST_HC_OPTS) -fno-code -fwrite-interface A.hs

# T20348A-E tested -dynamic-too output file options which no longer exist.
# -dynamic-too is deprecated and a no-op; all .o files are now dynamic-capable.
