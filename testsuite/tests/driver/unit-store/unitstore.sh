#!/bin/sh
# Unit store spike: build a few units once, then make them available to a
# client both through a ghc-pkg database (pkgdb/) and through a unit store
# (store/, via -unit-store), and check that the client behaves identically.
#
# The compiled artifacts are shared: the database entries point into
# store/<uid>/lib with ${pkgroot}-relative paths, the store finds the same
# directory by convention. Only the metadata route differs.
#
# Every unit is named by id, base included, so the compiler takes the
# ids-only path (Note [The ids-only unit state] in GHC.Unit.State) and never
# enumerates the store; the `closure` scenario contrasts it with the by-name
# path.
#
# Usage: TEST_HC=... TEST_HC_OPTS=... GHC_PKG=... AR=... sh unitstore.sh <scenario>
set -eu

scenario=$1

HC="$TEST_HC"
PKG="$GHC_PKG --no-user-package-db"
VER=$("$HC" --numeric-version)
BASE_ID=$($PKG field base id --simple-output)
HC_OPTS="$TEST_HC_OPTS -v0 -hide-all-packages -package-id $BASE_ID"

rm -rf store pkgdb obj out-db out-store
mkdir -p store pkgdb obj

# ---------------------------------------------------------------------------
# Writing metadata through both routes.

# conf <uid> <name> <version> <exposed-modules> <hidden-modules> <depends> [extra conf lines...]
conf() {
  uid=$1; name=$2; ver=$3; exposed=$4; hidden=$5; depends=$6; shift 6
  {
    printf 'name: %s\nversion: %s\nid: %s\nkey: %s\nexposed: True\n' "$name" "$ver" "$uid" "$uid"
    printf 'exposed-modules: %s\n' "$exposed"
    [ -z "$hidden" ] || printf 'hidden-modules: %s\n' "$hidden"
    printf 'import-dirs: ${pkgroot}/store/%s/lib\n' "$uid"
    printf 'library-dirs: ${pkgroot}/store/%s/lib\n' "$uid"
    printf 'dynamic-library-dirs: ${pkgroot}/store/%s/lib\n' "$uid"
    printf 'hs-libraries: HS%s\n' "$uid"
    [ -z "$depends" ] || printf 'depends: %s\n' "$(printf '%s' "$depends" | sed 's/ /, /g')"
    for l in "$@"; do printf '%s\n' "$l"; done
  } > "pkgdb/$uid.conf"
}

# meta <uid> <name> <version> <exposed-modules> <hidden-modules> <depends> [extra meta lines...]
meta() {
  uid=$1; name=$2; ver=$3; exposed=$4; hidden=$5; depends=$6; shift 6
  mkdir -p "store/$uid"
  {
    printf 'name: %s\nversion: %s\n' "$name" "$ver"
    printf 'exposed-modules: %s\n' "$exposed"
    [ -z "$hidden" ] || printf 'hidden-modules: %s\n' "$hidden"
    [ -z "$depends" ] || printf 'depends: %s\n' "$depends"
    for l in "$@"; do printf '%s\n' "$l"; done
  } > "store/$uid/unit.meta"
}

# both <uid> <name> <version> <exposed> <hidden> <depends>: identical metadata via both routes
both() { conf "$@"; meta "$@"; }

# ---------------------------------------------------------------------------
# Building units into the store layout.

# unit <uid> <srcdir> <ghc flags> <modules...>: compile and archive statically
unit() {
  uid=$1; src=$2; flags=$3; shift 3
  mkdir -p "store/$uid/lib" "obj/$uid"
  # shellcheck disable=SC2086
  "$HC" $HC_OPTS $flags -this-unit-id "$uid" -i"$src" -odir "obj/$uid" -hidir "store/$uid/lib" --make -no-link "$@"
  "$AR" qcS "store/$uid/lib/libHS$uid.a" $(find "obj/$uid" -name '*.o' | sort)
}

# dynunit <uid> <srcdir> <ghc flags> <modules...>: compile the dynamic way and link a shared library
dynunit() {
  uid=$1; src=$2; flags=$3; shift 3
  mkdir -p "store/$uid/lib" "obj/$uid"
  # shellcheck disable=SC2086
  "$HC" $HC_OPTS $flags -dynamic -osuf dyn_o -hisuf dyn_hi -this-unit-id "$uid" -i"$src" -odir "obj/$uid" -hidir "store/$uid/lib" --make -no-link "$@"
  # -rtsopts (from TEST_HC_OPTS) has no effect on a shared library and warns
  # shellcheck disable=SC2086
  "$HC" $(printf '%s' "$HC_OPTS" | sed 's/-rtsopts//g') $flags -dynamic -shared -o "store/$uid/lib/libHS$uid-ghc$VER.so" $(find "obj/$uid" -name '*.dyn_o' | sort)
}

# ---------------------------------------------------------------------------
# Running the client through both routes.

# client <Main.hs> <ghc flags>: compile through the db and through the store, run both, compare
client() {
  main=$1; flags=$2
  mkdir -p out-db out-store
  # shellcheck disable=SC2086
  "$HC" $HC_OPTS $flags -fforce-recomp -package-db pkgdb  -outputdir out-db    -o out-db/main    "$main"
  # shellcheck disable=SC2086
  "$HC" $HC_OPTS $flags -fforce-recomp -unit-store store -outputdir out-store -o out-store/main "$main"
  ./out-db/main    > out-db/run.txt
  ./out-store/main > out-store/run.txt
  cmp out-db/run.txt out-store/run.txt
  cat out-store/run.txt
}

# client_fails <Main.hs> <ghc flags>: both routes must fail with the same diagnostics
client_fails() {
  main=$1; flags=$2
  mkdir -p out-db out-store
  # shellcheck disable=SC2086
  if "$HC" $HC_OPTS $flags -fforce-recomp -package-db pkgdb  -outputdir out-db    -o out-db/main    "$main" 2> out-db/err.txt; then
    echo "db route: unexpected success"; exit 1
  fi
  # shellcheck disable=SC2086
  if "$HC" $HC_OPTS $flags -fforce-recomp -unit-store store -outputdir out-store -o out-store/main "$main" 2> out-store/err.txt; then
    echo "store route: unexpected success"; exit 1
  fi
  cmp out-db/err.txt out-store/err.txt
  cat out-store/err.txt
}

# ---------------------------------------------------------------------------
# Scenarios.

case $scenario in

  basic)
    # One unit, no dependencies beyond base.
    unit q-0.1 src "" Q
    both q-0.1 q 0.1 "Q" "" "$BASE_ID"
    $PKG recache --package-db pkgdb
    client MainBasic.hs "-package-id q-0.1"
    ;;

  multi)
    # p depends on q; q is linked through the dependency closure, not named.
    unit q-0.1 src "" Q
    both q-0.1 q 0.1 "Q" "" "$BASE_ID"
    $PKG recache --package-db pkgdb
    unit p-0.1 src "-package-db pkgdb -package-id q-0.1" P
    both p-0.1 p 0.1 "P" "P.Internal" "$BASE_ID q-0.1"
    $PKG recache --package-db pkgdb
    client MainMulti.hs "-package-id p-0.1"
    ;;

  hidden)
    # Importing a hidden module fails the same way through both routes.
    unit q-0.1 src "" Q
    both q-0.1 q 0.1 "Q" "" "$BASE_ID"
    $PKG recache --package-db pkgdb
    unit p-0.1 src "-package-db pkgdb -package-id q-0.1" P
    both p-0.1 p 0.1 "P" "P.Internal" "$BASE_ID q-0.1"
    $PKG recache --package-db pkgdb
    client_fails MainHidden.hs "-package-id p-0.1"
    ;;

  reexport)
    # r reexports Q from q; the client sees Q through r only.
    unit q-0.1 src "" Q
    both q-0.1 q 0.1 "Q" "" "$BASE_ID"
    $PKG recache --package-db pkgdb
    unit r-0.1 src "-package-db pkgdb -package-id q-0.1" R
    both r-0.1 r 0.1 "R Q from q-0.1:Q" "" "$BASE_ID q-0.1"
    $PKG recache --package-db pkgdb
    client MainReexport.hs "-package-id r-0.1"
    ;;

  safe)
    # Safe Haskell: units are untrusted by default through both routes, and
    # -trust-id makes them trusted through both routes.
    # S is Trustworthy, so its trust depends on the trust of its unit.
    unit s-0.1 src "-XHaskell2010" S
    both s-0.1 s 0.1 "S" "" "$BASE_ID"
    $PKG recache --package-db pkgdb
    client_fails MainSafe.hs "-XHaskell2010 -package-id s-0.1 -fpackage-trust -trust-id $BASE_ID"
    client MainSafe.hs "-XHaskell2010 -package-id s-0.1 -fpackage-trust -trust-id $BASE_ID -trust-id s-0.1"
    ;;

  dynamic)
    # Dynamic way: .dyn_hi files and libHS<uid>-ghc<ver>.so in lib/.
    dynunit q-0.1 src "" Q
    both q-0.1 q 0.1 "Q" "" "$BASE_ID"
    $PKG recache --package-db pkgdb
    client MainBasic.hs "-dynamic -package-id q-0.1"
    ;;

  envfile)
    # The unit-store directive in an environment file, relative to the file.
    unit q-0.1 src "" Q
    both q-0.1 q 0.1 "Q" "" "$BASE_ID"
    $PKG recache --package-db pkgdb
    mkdir -p envs
    printf 'package-db ../pkgdb\npackage-id %s\npackage-id q-0.1\n' "$BASE_ID" > envs/db.env
    printf 'unit-store ../store\npackage-id %s\npackage-id q-0.1\n' "$BASE_ID" > envs/store.env
    mkdir -p out-db out-store
    # shellcheck disable=SC2086
    "$HC" $TEST_HC_OPTS -v0 -fforce-recomp -package-env envs/db.env    -outputdir out-db    -o out-db/main    MainBasic.hs
    # shellcheck disable=SC2086
    "$HC" $TEST_HC_OPTS -v0 -fforce-recomp -package-env envs/store.env -outputdir out-store -o out-store/main MainBasic.hs
    ./out-db/main    > out-db/run.txt
    ./out-store/main > out-store/run.txt
    cmp out-db/run.txt out-store/run.txt
    cat out-store/run.txt
    ;;

  backpack)
    # An indefinite unit `ind` with signature H, instantiated by hand with
    # q's Q as `ind+q`, registered without Cabal through both routes.
    unit q-0.1 src "" Q
    both q-0.1 q 0.1 "Q" "" "$BASE_ID"
    $PKG recache --package-db pkgdb
    # the indefinite unit: typechecked only, no code
    mkdir -p store/ind/lib obj/ind
    # shellcheck disable=SC2086
    "$HC" $HC_OPTS -this-unit-id ind -this-component-id ind -instantiated-with 'H=<H>' -isrc/bkp \
      -odir obj/ind -hidir store/ind/lib --make -no-link H UseH
    both ind ind 0.1 "H UseH" "" "$BASE_ID" "indefinite: True" "instantiated-with: H=<H>"
    $PKG recache --package-db pkgdb
    # the instantiated unit: compiled against q's Q
    unit ind+q src/bkp "-package-db pkgdb -package-id q-0.1 -this-component-id ind -instantiated-with H=q-0.1:Q" H UseH
    both ind+q ind 0.1 "H UseH" "" "$BASE_ID q-0.1" "instantiated-with: H=q-0.1:Q"
    $PKG recache --package-db pkgdb
    client MainBackpack.hs "-package-id q-0.1 -package-id ind+q"
    ;;

  demand)
    # Demand-driven reading: with -hide-all-packages and -package-id flags the
    # store is probed for the units needed and never enumerated. A unit with a
    # malformed manifest sits in the store; the client builds anyway, and only
    # an eager read (no -hide-all-packages) trips over it.
    unit q-0.1 src "" Q
    both q-0.1 q 0.1 "Q" "" "$BASE_ID"
    $PKG recache --package-db pkgdb
    unit p-0.1 src "-package-db pkgdb -package-id q-0.1" P
    both p-0.1 p 0.1 "P" "P.Internal" "$BASE_ID q-0.1"
    $PKG recache --package-db pkgdb
    mkdir -p store/broken-0.1
    printf 'this is not a manifest\n' > store/broken-0.1/unit.meta
    client MainMulti.hs "-package-id p-0.1"
    # shellcheck disable=SC2086
    if "$HC" $TEST_HC_OPTS -v0 -fforce-recomp -unit-store store -outputdir out-eager -o out-eager/main MainMulti.hs 2> out-eager.err; then
      echo "eager read: unexpected success"; exit 1
    fi
    if grep -q 'malformed' out-eager.err; then
      echo "eager read fails on the malformed manifest"
    else
      cat out-eager.err; exit 1
    fi
    ;;

  closure)
    # The ids-only unit state (Note [The ids-only unit state] in
    # GHC.Unit.State): only the closure of the units named by id exists.
    # s-0.1 is installed through both routes but not reachable from q-0.1,
    # so importing S fails as "not found" rather than "hidden", identically
    # through both routes.
    unit q-0.1 src "" Q
    both q-0.1 q 0.1 "Q" "" "$BASE_ID"
    unit s-0.1 src "-XHaskell2010" S
    both s-0.1 s 0.1 "S" "" "$BASE_ID"
    $PKG recache --package-db pkgdb
    client_fails MainClosure.hs "-package-id q-0.1"
    # A package name on the command line selects the by-name path, which
    # sees everything installed and reports s-0.1 as hidden.
    HC_OPTS="$TEST_HC_OPTS -v0 -hide-all-packages -package base"
    client_fails MainClosure.hs "-package-id q-0.1"
    ;;

  *)
    echo "unknown scenario: $scenario" >&2
    exit 2
    ;;
esac
