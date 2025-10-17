# configure.ac
AC_PREREQ([2.69])
AC_INIT([ghc-builder], [0.1.0], [your-email@example.com])
AC_CONFIG_SRCDIR([.]) # A representative .in file
#AC_CONFIG_AUX_DIR([build-aux]) # Recommended place for config.guess, config.sub
AC_CONFIG_MACRO_DIR([m4])      # For any custom m4 macros
# AM_INIT_AUTOMAKE([-Wall -Werror foreign]) # Using some automake conventions

# --- Feature toggle (dynamic only) for imported project settings ---
AC_ARG_ENABLE([dynamic],
        [AS_HELP_STRING([--enable-dynamic],[Build shared libraries and enable dynamic RTS])],
        [enable_dynamic=$enableval],[enable_dynamic=no])

# Initialize accumulation variables
ALL_PACKAGES=""
CONSTRAINTS=""

# Provide defaults first (static)
APPEND_PKG_FIELD([shared: False])
APPEND_PKG_FIELD([executable-dynamic: False])

AS_IF([test "x$enable_dynamic" = "xyes"], [
        # Override (we rebuild list to avoid mixing static+dynamic lines)
        ALL_PACKAGES=""
        APPEND_PKG_FIELD([shared: True])
        APPEND_PKG_FIELD([executable-dynamic: True])
        APPEND_CONSTRAINT([rts +dynamic])
])

# Indent with two spaces for substitution blocks (uniform handling)
ALL_PACKAGES=`printf '%b' "$ALL_PACKAGES"`
CONSTRAINTS=`printf '%b' "$CONSTRAINTS"`
ALL_PACKAGES_INDENTED=`printf '%s' "$ALL_PACKAGES" | sed 's/^/  /'`
CONSTRAINTS_INDENTED=`printf '%s' "$CONSTRAINTS" | sed 's/^/  /'`
ALL_PACKAGES="$ALL_PACKAGES_INDENTED"
AS_IF([test "x$CONSTRAINTS" = "x"], [CONSTRAINTS="  -- (none)"], [CONSTRAINTS="$CONSTRAINTS_INDENTED"])

AC_SUBST([ALL_PACKAGES])
AC_SUBST([CONSTRAINTS])

# --- Define Programs ---
# We don't need to check for CC, MAKE_SET, and others for now, we only want substitution.
# AC_PROG_CC
# AC_PROG_MAKE_SET # To ensure 'set' works in Makefiles for undefined variables
AC_CHECK_PROGS([PYTHON], [python3 python python2])
AS_IF([test "x$PYTHON" = x], [AC_MSG_ERROR([Python interpreter not found. Please install Python or set PYTHON environment variable.])])
AC_SUBST([PYTHON])

# FIXME: For now we keep this check, but just widen it aggressively.
#        At some point we should just outright remove this.
LlvmMinVersion=10 # inclusive
LlvmMaxVersion=100 # not inclusive
AC_SUBST([LlvmMinVersion])
AC_SUBST([LlvmMaxVersion])

# --- Files to generate ---
# config.status will create these files by substituting @VAR@ placeholders.
AC_CONFIG_FILES([
    compiler/GHC/CmmToLlvm/Version/Bounds.hs:compiler/GHC/CmmToLlvm/Version/Bounds.hs.in
    cabal.project.stage2.settings:cabal.project.stage2.settings.in
])

AC_OUTPUT

# After running ./configure, the following command can be used to see configured values:
# ./config.status --config
