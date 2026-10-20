TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: profiling_target_metadata
profiling_target_metadata:
	$(PYTHON) profiling_target_metadata.py --testsuite-root "$(TOP_ABS)" --make "$(MAKE)"
