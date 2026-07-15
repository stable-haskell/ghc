TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

BASE_VERSION = $('$GHC_PKG' field base id --simple-output)


a.out: Main.o M.o
	'$(TEST_HC)' $(EXTRA_HC_OPTS) Main.o M.o -package-env -

Main.o Main.hi: M.hi hsdep/pkgdb/package.cache hsdep/HsDep.hi hsdep/libHShsdep-0.1-ghc8.10.7.so
	'$(TEST_HC)' $(EXTRA_HC_OPTS) -c Main.hs hsdep/libHShsdep-0.1-ghc8.10.7.so -i. -package-env - -package-db hsdep/pkgdb

M.o M.hi: M.hs hsdep-empty-lib/pkgdb/package.cache hsdep/HsDep.hi hsdep-empty-lib/libHShsdep-0.1-ghc8.10.7.so
	'$(TEST_HC)' $(EXTRA_HC_OPTS) -c M.hs hsdep/HsDep.o -package-env - -package-db hsdep-empty-lib/pkgdb

hsdep/pkgdb/package.cache: cat-hsdep-info.sh
	mkdir -p hsdep/pkgdb
	./cat-hsdep-info.sh hsdep $(BASE_VERSION) > hsdep/pkgdb/hsdep-0.1.conf
	'$(GHC_PKG)' recache --package-db hsdep/pkgdb

hsdep-empty-lib/pkgdb/package.cache: cat-hsdep-info.sh
	mkdir -p hsdep-empty-lib/pkgdb
	GHC_PKG='$(GHC_PKG)' ./cat-hsdep-info.sh hsdep-empty-lib > hsdep-empty-lib/pkgdb/hsdep-0.1.conf
	'$(GHC_PKG)' recache --package-db hsdep-empty-lib/pkgdb

# All .o files are now dynamic-capable, so use .o directly for shared lib
hsdep/libHShsdep-0.1-ghc8.10.7.so: hsdep/HsDep.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -shared -dynamic -o hsdep/libHShsdep-0.1-ghc8.10.7.so hsdep/HsDep.o

hsdep-empty-lib/libHShsdep-0.1-ghc8.10.7.so:
	mkdir -p hsdep-empty-lib
	touch empty.c
	'$(TEST_HC)' $(EXTRA_HC_OPTS) -shared -dynamic -o hsdep-empty-lib/libHShsdep-0.1-ghc8.10.7.so empty.c
	rm empty.c

# Compile without -dynamic: all .o files are now dynamic-capable,
# and the .hi profile tag must be "" (not "dyn") for non-dynamic consumers.
hsdep/HsDep.hi hsdep/HsDep.o: hsdep/HsDep.hs
	'$(TEST_HC)' $(EXTRA_HC_OPTS) -c -this-unit-id hsdep-0.1 hsdep/HsDep.hs

T21035: a.out

clean:
	find . \( -name "*.*hi" -o -name "*.*o" \) -delete
	rm -rf hsdep/pkgdb hsdep-empty-lib a.out
