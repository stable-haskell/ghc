TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test that -dynamic-too produces a deprecation warning

checkExists = [ -f $1 ] || echo $1 missing
checkNotExists = [ ! -f $1  ] || echo $1 not missing

clean:
	rm -f *.o
	rm -f *.hi

T20436: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) -dynamic -dynamic-too -v0 A.hs
	$(call checkExists,A.hi)
	$(call checkExists,A.o)
	$(call checkNotExists,A.dyn_hi)
	$(call checkNotExists,A.dyn_o)
