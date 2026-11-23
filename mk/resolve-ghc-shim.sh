# Resolve a GHCup or Scoop shim to the real compiler.
#
# Cabal 3.19 passes the compiler as an 8.3 short path. The shim then looks
# for "<short>.shim", which GHCup never wrote, and prints
# "Cannot open shim file for read." The .shim file sits beside the long
# name and contains "path = <real ghc>" with no trailing newline.
#
# Source this from a configure script. It defines trim_ws and shim_target.
# shim_target prints the real compiler, or nothing.

trim_ws() {
  s=$1
  s=${s#"${s%%[![:space:]]*}"}
  s=${s%"${s##*[![:space:]]}"}
  printf '%s\n' "$s"
}

shim_target() {
  cmd=$1
  path=$cmd
  case $cmd in
    */*|*\\*) ;;
    *)
      path=$(command -v "$cmd" 2>/dev/null || true)
      [ -n "$path" ] || return 0
      ;;
  esac

  if command -v cygpath >/dev/null 2>&1; then
    win=$(cygpath -w -l -- "$path" 2>/dev/null | tr -d '\r' || true)
    if [ -n "$win" ]; then
      unix=$(cygpath -u -- "$win" 2>/dev/null | tr -d '\r' || true)
      [ -n "$unix" ] && path=$unix
    fi
  fi

  dir=$(dirname "$path")
  base=$(basename "$path")
  case $base in
    *.exe) stem=${base%.exe} ;;
    *.EXE) stem=${base%.EXE} ;;
    *) stem=$base ;;
  esac

  shim=
  if [ -f "$dir/$stem.shim" ]; then
    shim=$dir/$stem.shim
  elif [ -f "$dir/$base.shim" ]; then
    shim=$dir/$base.shim
  else
    return 0
  fi

  target=
  while IFS= read -r line || [ -n "${line:-}" ]; do
    line=$(printf '%s' "$line" | tr -d '\r')
    case $line in
      path*)
        rest=${line#path}
        rest=$(trim_ws "$rest")
        case $rest in
          =*)
            target=$(trim_ws "${rest#=}")
            break
            ;;
        esac
        ;;
    esac
  done < "$shim"
  [ -n "$target" ] || return 0
  case $target in
    \"*\") target=${target#\"}; target=${target%\"} ;;
    \'*\') target=${target#\'}; target=${target%\'} ;;
  esac
  if command -v cygpath >/dev/null 2>&1; then
    unix=$(cygpath -u -- "$target" 2>/dev/null | tr -d '\r' || true)
    [ -n "$unix" ] && target=$unix
  fi
  printf '%s\n' "$target"
}
