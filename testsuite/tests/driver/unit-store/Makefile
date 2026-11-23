TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Every scenario is driven by unitstore.sh; see the comments there.
export TEST_HC TEST_HC_OPTS GHC_PKG AR

unitstore_%:
	sh unitstore.sh $*
