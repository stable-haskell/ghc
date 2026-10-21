#!/usr/bin/env bash
set -euo pipefail

script=$(cd "$(dirname "$0")/.." && pwd)/clean-darwin-macho.sh
scratch=$(mktemp -d "${TMPDIR:-/tmp}/macho-rpath-test.XXXXXX")
trap 'rm -rf "$scratch"' EXIT
host_os=$(uname -s)
system_bash=$(command -v bash)
system_uname=$(command -v uname)
system_find=$(command -v find)
system_grep=$(command -v grep)
system_awk=$(command -v awk)
system_file=$(command -v file)
system_otool=$(command -v otool 2>/dev/null || true)
system_install_name_tool=$(command -v install_name_tool 2>/dev/null || true)
system_clang=$(command -v clang 2>/dev/null || true)
system_ln=$(command -v ln)
system_mkdir=$(command -v mkdir)

mock_bin="$scratch/mock tools"
build_root="$scratch/path inside root/work space/_build"
dist_dir="$scratch/dist space"
mkdir -p "$mock_bin" "$build_root/stage2/store" "$dist_dir/bin"
binary="$dist_dir/bin/compiler"
: >"$binary"
chmod +x "$binary"

cat >"$mock_bin/uname" <<'EOF'
#!/usr/bin/env bash
printf 'Darwin\n'
EOF
cat >"$mock_bin/file" <<'EOF'
#!/usr/bin/env bash
printf '%s: Mach-O 64-bit executable\n' "$2"
EOF
cat >"$mock_bin/otool" <<'EOF'
#!/usr/bin/env bash
if [ "$1" = -l ]; then
  if [ "${FAIL_OTOOL:-0}" = 1 ]; then
    exit 1
  fi
  cat "$MOCK_RPATHS"
else
  printf '%s:\n' "$2"
fi
EOF
cat >"$mock_bin/install_name_tool" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$@" >>"$INSTALL_LOG"
if [ "${FAIL_DELETE:-0}" = 1 ] && [ "$1" = -delete_rpath ]; then
  exit 1
fi
EOF
cat >"$mock_bin/codesign" <<'EOF'
#!/usr/bin/env bash
if [ "${FAIL_CODESIGN:-0}" = 1 ]; then
  printf 'mock codesign stdout\n'
  printf 'mock codesign diagnostic\n' >&2
  exit 1
fi
EOF
chmod +x "$mock_bin"/*

rpaths="$scratch/otool output"
{
  printf '          cmd LC_RPATH\n'
  printf '      cmdsize 64\n'
  printf '         path %s/stage2/store with spaces/lib (offset 12)\n' "$build_root"
  printf '          cmd LC_RPATH\n'
  printf '      cmdsize 40\n'
  printf '         path /opt/external toolchain/lib (offset 12)\n'
  printf '          cmd LC_RPATH\n'
  printf '      cmdsize 40\n'
  printf '         path @loader_path/../lib (offset 12)\n'
  printf '          cmd LC_RPATH\n'
  printf '      cmdsize 40\n'
  printf '         path /nix/store/example/lib (offset 12)\n'
} >"$rpaths"

export PATH="$mock_bin:$PATH"
export MOCK_RPATHS="$rpaths"
export INSTALL_LOG="$scratch/install-name.log"
: >"$INSTALL_LOG"
"$script" "$dist_dir" "$build_root" >"$scratch/success.log"
grep -Fx -- '-delete_rpath' "$INSTALL_LOG" >/dev/null
grep -Fx -- "$build_root/stage2/store with spaces/lib" "$INSTALL_LOG" >/dev/null
if grep -F '/opt/external' "$INSTALL_LOG" || grep -F '@loader_path' "$INSTALL_LOG" || grep -F '/nix/store' "$INSTALL_LOG"; then
  echo "unrelated LC_RPATH was modified" >&2
  exit 1
fi
: >"$INSTALL_LOG"
if FAIL_DELETE=1 "$script" "$dist_dir" "$build_root" >"$scratch/delete-failure.log" 2>&1; then
  echo "expected build-root RPATH deletion failure" >&2
  exit 1
fi
grep -F "failed to delete build-root LC_RPATH" "$scratch/delete-failure.log" >/dev/null

if FAIL_OTOOL=1 "$script" "$dist_dir" "$build_root" >"$scratch/otool-failure.log" 2>&1; then
  echo "expected LC_RPATH inspection failure" >&2
  exit 1
fi
grep -F "failed to inspect LC_RPATH entries" "$scratch/otool-failure.log" >/dev/null

if FAIL_CODESIGN=1 "$script" "$dist_dir" "$build_root" >"$scratch/codesign-failure.log" 2>&1; then
  echo "expected ad-hoc signing failure" >&2
  exit 1
fi
grep -F "failed to ad-hoc sign modified Mach-O" "$scratch/codesign-failure.log" >/dev/null
grep -F "mock codesign diagnostic" "$scratch/codesign-failure.log" >/dev/null

if [ "$host_os" = Darwin ] && [ -x /usr/bin/codesign ] \
   && [ -n "$system_otool" ] && [ -n "$system_install_name_tool" ] \
   && [ -n "$system_clang" ]; then
  # Exercise the system-tool fallback against a real private Mach-O copy. The
  # reduced PATH has the inspection/mutation tools but deliberately omits
  # codesign, as devx does on the Darwin CI runners.
  real_tools="$scratch/real tools"
  real_dist="$scratch/real dist"
  real_build_root="$scratch/real build/_build"
  real_binary="$real_dist/bin/probe"
  "$system_mkdir" -p "$real_tools" "$real_dist/bin" "$real_build_root"
  printf 'int main(void) { return 0; }\n' >"$scratch/probe.c"
  "$system_clang" -Wl,-headerpad_max_install_names -o "$real_binary" "$scratch/probe.c"
  "$system_install_name_tool" -add_rpath "$real_build_root/ci-probe" "$real_binary"
  for tool in uname find grep awk file otool install_name_tool; do
    case "$tool" in
      uname) tool_path=$system_uname ;;
      find) tool_path=$system_find ;;
      grep) tool_path=$system_grep ;;
      awk) tool_path=$system_awk ;;
      file) tool_path=$system_file ;;
      otool) tool_path=$system_otool ;;
      install_name_tool) tool_path=$system_install_name_tool ;;
    esac
    "$system_ln" -s "$tool_path" "$real_tools/$tool"
  done
  if PATH="$real_tools" "$system_bash" -c 'command -v codesign' >/dev/null 2>&1; then
    echo "codesign unexpectedly exists in the fallback test PATH" >&2
    exit 1
  fi
  PATH="$real_tools" "$system_bash" "$script" "$real_dist" "$real_build_root" \
    >"$scratch/system-codesign-fallback.log"
  /usr/bin/codesign --verify "$real_binary"
  if "$system_otool" -l "$real_binary" | "$system_grep" -F "$real_build_root/ci-probe" >/dev/null; then
    echo "build-root LC_RPATH survived system codesign fallback" >&2
    exit 1
  fi
fi

echo "Mach-O RPATH scope, spaces, and failure handling passed"
