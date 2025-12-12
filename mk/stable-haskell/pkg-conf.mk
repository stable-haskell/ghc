MK_PKG_CONF := 1

# patchpackageconf
#
# Hacky function to patch up the paths in the package .conf files
#
# $1 = package name (ex: 'bytestring')
# TODO: package name is borked for sublibs
# $2 = path to .conf file
# $3 = (relative) path from $${pkgroot} to docs directory
# $4 = host triple
# $5 = package name and version (ex: bytestring-0.13)
#
# NOTE: We must make sure we keep sub-folder structures alive.  There might be
#       references to $5/build/FOO, we must keep /FOO at the end.  One thing not
#       retaining this that will break are pubilc sublibraries.
#
# FIXME: cabal should just be able to create .conf file properly relocated.  And
#        allow us to install them into a pre-defined package-db, this would
#        eliminate this nonsense.
define patchpackageconf
    case $5 in \
		rts-*-nonthreaded-nodebug) \
	      sublib="/nonthreaded-nodebug" ;; \
		rts-*-nonthreaded-debug) \
	      sublib="/nonthreaded-debug" ;; \
		rts-*-threaded-nodebug) \
	      sublib="/threaded-nodebug" ;; \
		rts-*-threaded-debug) \
	      sublib="/threaded-debug" ;; \
		*) \
		  sublib="" ;; \
	esac ; \
	$(SED) -i \
		-e "s|haddock-interfaces:.*|haddock-interfaces: \"\$${pkgroot}/$3/html/libraries/$5/$1.haddock\"|" \
		-e "s|haddock-html:.*|haddock-html: \"\$${pkgroot}/$3/html/libraries/$5\"|" \
        -e "s|import-dirs:.*|import-dirs: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
		-e "s|library-dirs:.*|library-dirs: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
		-e "s|library-dirs-static:.*|library-dirs-static: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
		-e "s|dynamic-library-dirs:.*|dynamic-library-dirs: \"\$${pkgroot}/../lib/$4\"|" \
		-e "s|data-dir:.*|data-dir: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
		-e "s|include-dirs:.*|include-dirs: \"\$${pkgroot}/../lib/$4/$5$${sublib}/include\"|" \
		-e "s|^    /.*||" \
		-e "s|^    [A-Z]:.*||" \
		$2
endef
