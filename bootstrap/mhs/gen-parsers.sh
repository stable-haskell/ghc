#!/usr/bin/env bash
#
# Generate MicroHs-compatible Happy parsers and Alex lexers for stage1.
#
# Outputs (OUT_AUTOGEN defaults to _build/mhs-stage1/autogen):
#   $OUT_AUTOGEN/GHC/Parser.hs                <- compiler/GHC/Parser.y
#   $OUT_AUTOGEN/GHC/Parser/Lexer.hs          <- compiler/GHC/Parser/Lexer.x
#   $OUT_AUTOGEN/GHC/Parser/Lexer/String.hs  <- compiler/GHC/Parser/Lexer/String.x
#   $OUT_AUTOGEN/GHC/Parser/HaddockLex.hs     <- compiler/GHC/Parser/HaddockLex.x
#   $OUT_AUTOGEN/GHC/Cmm/Parser.hs            <- compiler/GHC/Cmm/Parser.y
#   $OUT_AUTOGEN/GHC/Cmm/Lexer.hs            <- compiler/GHC/Cmm/Lexer.x
#   $OUT_AUTOGEN/genprimopcode/Parser.hs      <- utils/genprimopcode/Parser.y
#   $OUT_AUTOGEN/genprimopcode/Lexer.hs       <- utils/genprimopcode/Lexer.x
#
# Pinned tools (MANDATORY: happy 2.1.7 from PATH emits Happy_GHC_Exts.MutVar#
# even without -g, so it must not be used):
#   happy 2.1.5, WITHOUT -g/-c/-a.  NOTE: happy 2.x has no pure-Haskell98
#   backend (the --ghc backend is non-optional), so its raw output still uses
#   Happy_GHC_Exts.Int#/Addr# tables.  mhsify_happy below rewrites the
#   template's unboxed machinery to boxed Data.Array tables + Int arithmetic,
#   because MicroHs 0.16.12.0 lacks +#/-#/*/#/>=#/negateInt#/chr#/ord# and the
#   whole index*OffAddr# family.  (It does support I#/Int# syntax, unboxed
#   tuples/sums, and most other GHC.Exts primops, so handwritten code using
#   only those needs no changes.)
#   alex 3.5.4.0, WITHOUT -g (boxed Data.Array tables).  --latin1 is passed
#   for the compiler package only, matching Hadrian
#   (hadrian/src/Settings/Packages.hs: builder Alex ? arg "--latin1");
#   utils/genprimopcode uses alex defaults, also matching Hadrian.
#
# Idempotent: an output is regenerated only when it is missing or older than
# its .x/.y source or this script.
#
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT_PATH="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/$(basename "${BASH_SOURCE[0]}")"
OUT_AUTOGEN="${1:-$ROOT/_build/mhs-stage1/autogen}"

HAPPY_BIN="${HAPPY_BIN:-/nix/store/zlh6z044flfimz0vrdrrgalxs0nnklfy-happy-exe-happy-2.1.5/bin/happy}"
if [ ! -x "$HAPPY_BIN" ]; then
  HAPPY_BIN="$(ls -d /nix/store/*happy-2.1.5*/bin/happy 2>/dev/null | head -n 1 || true)"
fi
if [ ! -x "$HAPPY_BIN" ]; then
  HAPPY_BIN="$(command -v happy || true)"
fi
if [ ! -x "$HAPPY_BIN" ]; then
  echo "gen-parsers.sh: error: happy 2.1.5 not found" >&2
  exit 1
fi

ALEX_BIN="${ALEX_BIN:-/nix/store/3iphln0wbab5v1rp1lbvhzn6q69sab1l-alex-3.5.4.0/bin/alex}"
if [ ! -x "$ALEX_BIN" ]; then
  ALEX_BIN="$(ls -d /nix/store/*alex-3.5.4.0*/bin/alex 2>/dev/null | head -n 1 || true)"
fi
if [ ! -x "$ALEX_BIN" ]; then
  ALEX_BIN="$(command -v alex || true)"
fi
if [ ! -x "$ALEX_BIN" ]; then
  echo "gen-parsers.sh: error: alex 3.5.4.0 not found" >&2
  exit 1
fi

if ! command -v python3 >/dev/null 2>&1; then
  echo "gen-parsers.sh: error: python3 is required for the Happy postprocess" >&2
  exit 1
fi

echo "gen-parsers.sh: happy=$HAPPY_BIN"
echo "gen-parsers.sh: alex=$ALEX_BIN"
HAPPY_VER="$("$HAPPY_BIN" --version | head -n 1)"
ALEX_VER="$("$ALEX_BIN" --version | head -n 1)"
echo "$HAPPY_VER"
echo "$ALEX_VER"
case "$HAPPY_VER" in
  *"Happy Version 2.1.5"*) ;;
  *)
    if [ "${ALLOW_UNPINNED_HAPPY:-0}" != "1" ]; then
      echo "gen-parsers.sh: error: Happy 2.1.5 is required (got: $HAPPY_VER); set ALLOW_UNPINNED_HAPPY=1 to override" >&2
      exit 1
    fi
    ;;
esac

is_fresh() {
  [ -n "${1:-}" ] && [ -n "${2:-}" ] \
    && [ -f "$1" ] && [ "$1" -nt "$2" ] && [ "$1" -nt "$SCRIPT_PATH" ]
}

# mhsify_happy TMP DST: rewrite Happy 2.1.5 template primops in TMP to boxed
# equivalents in DST.  Fails loudly on any unexpected template shape.
mhsify_happy() {
python3 - "$1" "$2" <<'PYEOF'
import re
import sys

src_path, dst_path = sys.argv[1], sys.argv[2]

with open(src_path, 'r', encoding='utf-8') as f:
    lines = f.read().split('\n')

def die(msg):
    sys.stderr.write('mhsify-happy: error: %s (file %s)\n' % (msg, src_path))
    sys.exit(1)

# 1. Drop GHC-only scaffolding: MachDeps.h include, the
#    "#if !defined(__GLASGOW_HASKELL__) / #error / #endif" stanza, and the
#    ">= 710 / PartialTypeSignatures" stanza.
out = []
i = 0
while i < len(lines):
    ln = lines[i]
    if ln in ('#include "MachDeps.h"', '#include "ghcconfig.h"'):
        i += 1
        continue
    if ln == '#if !defined(__GLASGOW_HASKELL__)':
        if lines[i + 1:i + 3] != ["#  error This code isn't being built with GHC.",
                                  '#endif']:
            die('unexpected __GLASGOW_HASKELL__ guard at line %d' % (i + 1))
        i += 3
        continue
    if ln == '#if __GLASGOW_HASKELL__ >= 710':
        if lines[i + 1:i + 3] != ['{-# LANGUAGE PartialTypeSignatures #-}',
                                  '#endif']:
            die('unexpected >= 710 stanza at line %d' % (i + 1))
        i += 3
        continue
    out.append(ln)
    i += 1
lines = out

# 2. Exact-line rewrites of macros, tokens and type synonyms.
old_new = [
    ('#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Happy_Prelude.Bool)',
     '#define LT(n,m) ((n) Happy_Prelude.< (m))'),
    ('#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Happy_Prelude.Bool)',
     '#define GTE(n,m) ((n) Happy_Prelude.>= (m))'),
    ('#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Happy_Prelude.Bool)',
     '#define EQ(n,m) ((n) Happy_Prelude.== (m))'),
    ('#define PLUS(n,m) (n Happy_GHC_Exts.+# m)',
     '#define PLUS(n,m) ((n) Happy_Prelude.+ (m))'),
    ('#define MINUS(n,m) (n Happy_GHC_Exts.-# m)',
     '#define MINUS(n,m) ((n) Happy_Prelude.- (m))'),
    ('#define TIMES(n,m) (n Happy_GHC_Exts.*# m)',
     '#define TIMES(n,m) ((n) Happy_Prelude.* (m))'),
    ('#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))',
     '#define NEGATE(n) (Happy_Prelude.negate (n))'),
    ('#define INVALID_TOK -1#', '#define INVALID_TOK (-1)'),
    ('#define ERROR_TOK 0#', '#define ERROR_TOK (0)'),
    ('#define CATCH_TOK 1#', '#define CATCH_TOK (1)'),
    ('type Happy_Int = Happy_GHC_Exts.Int#',
     'type Happy_Int = Happy_Prelude.Int'),
    ('data HappyAddr = HappyA# Happy_GHC_Exts.Addr#',
     'data HappyAddr = HappyA# (Happy_Data_Array.Array Happy_Prelude.Int Happy_Prelude.Int)'),
    ('import qualified GHC.Exts as Happy_GHC_Exts', None),
    # The template aliases GHC.Num/GHC.Err as Happy_Prelude, but MicroHs
    # has neither module. Every Happy_Prelude.X actually used in the
    # generated drivers is a plain Prelude name, so alias Prelude instead.
    # Note: in Haskell 2010 an explicit `import qualified Prelude` suppresses
    # the implicit unqualified `import Prelude`, so keep implicit Prelude for
    # non-GHC.Prelude grammars (e.g. genprimopcode).
    ('import qualified GHC.Num as Happy_Prelude',
     'import qualified Prelude as Happy_Prelude'
     if any('GHC.Prelude' in l for l in lines)
     else 'import Prelude\nimport qualified Prelude as Happy_Prelude'),
    ('import qualified GHC.Err as Happy_Prelude', None),
]
for old, new in old_new:
    hits = [k for k, ln in enumerate(lines) if ln == old]
    if len(hits) != 1:
        die('expected 1 occurrence of %r, found %d' % (old, len(hits)))
    if new is None:
        del lines[hits[0]]
    else:
        lines[hits[0]] = new

# 3. HAPPY_COERCE block: keep the boxed (non-coerce) branch unconditionally.
#    (We never pass -c, but normalise both branches so no unsafeCoerce# stays.)
try:
    ci = lines.index('#if defined(HAPPY_COERCE)')
except ValueError:
    die('HAPPY_COERCE block not found')
coerce_old = [
    '#if defined(HAPPY_COERCE)',
    '#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# i) -> i })',
    '#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# i))',
    '#  define MK_TOKEN(x)         (happyInTok (x))',
    '#else',
    '#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i })',
    '#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (Happy_GHC_Exts.I# i))',
    '#  define MK_TOKEN(x)         (HappyTerminal (x))',
    '#endif',
]
if lines[ci:ci + 9] != coerce_old:
    die('HAPPY_COERCE block has unexpected shape')
lines[ci:ci + 9] = [
    '#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken i -> i })',
    '#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (i))',
    '#  define MK_TOKEN(x)         (HappyTerminal (x))',
]

# 4. happyIndexOffAddr: Addr#/indexInt32OffAddr# indexing becomes a boxed
#    Data.Array lookup.  (Little-endian host assumed, matching the template's
#    non-WORDS_BIGENDIAN path that Hadrian builds use.)
try:
    ii = lines.index('{-# INLINE happyIndexOffAddr #-}')
except ValueError:
    die('happyIndexOffAddr not found')
j = ii
while True:
    j += 1
    if j >= len(lines):
        die('unterminated happyIndexOffAddr block')
    if lines[j] == '#endif' and any('indexInt32OffAddr#' in lines[k]
                                    for k in range(ii, j)):
        break
if 'happyIndexOffAddr (HappyA# arr) off =' not in '\n'.join(lines[ii:j + 1]):
    die('happyIndexOffAddr block has unexpected shape')
lines[ii:j + 1] = [
    '{-# INLINE happyIndexOffAddr #-}',
    'happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int',
    'happyIndexOffAddr (HappyA# arr) off = arr Happy_Data_Array.! off',
]

# 5. HappyA# "..." tables (NUL-padded blobs of little-endian int32 words)
#    become Data.Array.listArray tables of boxed Ints.
table_re = re.compile(r'^([A-Za-z_]\w*) = HappyA# "(.*)"#\s*$')
dec_re = re.compile(r'[0-9]+')

def decode_table(s):
    out = bytearray()
    k = 0
    while k < len(s):
        c = s[k]
        if c != '\\':
            o = ord(c)
            if o > 127:
                die('non-ASCII byte in HappyA# table')
            out.append(o)
            k += 1
            continue
        if k + 1 >= len(s):
            die('trailing backslash in HappyA# table')
        e = s[k + 1]
        if e == 'x':
            if k + 3 >= len(s):
                die('truncated \\x escape in HappyA# table')
            try:
                out.append(int(s[k + 2:k + 4], 16))
            except ValueError:
                die('bad \\x escape in HappyA# table')
            k += 4
        elif e == 'n':
            out.append(10); k += 2
        elif e == 'r':
            out.append(13); k += 2
        elif e == 't':
            out.append(9); k += 2
        elif e == '\\':
            out.append(92); k += 2
        elif e == '"':
            out.append(34); k += 2
        elif e == "'":
            out.append(39); k += 2
        elif e == '0':
            out.append(0); k += 2
        elif e.isdigit():
            m = dec_re.match(s, k + 1)
            out.append(int(m.group(0)) % 256)
            k += 1 + len(m.group(0))
        elif e == '\n' or e == '&':
            k += 2
        else:
            die('unsupported escape %r in HappyA# table' % ('\\' + e))
    if len(out) % 4 != 0:
        die('HappyA# table size %d is not a multiple of 4' % len(out))
    return [int.from_bytes(bytes(out[t:t + 4]), 'little', signed=True)
            for t in range(0, len(out), 4)]

def emit_table(name, vals):
    head = ('%s = HappyA# (Happy_Data_Array.listArray (0 :: Happy_Prelude.Int, %d)'
            % (name, len(vals) - 1))
    if not vals:
        return [head + ' [])']
    chunks = [vals[p:p + 12] for p in range(0, len(vals), 12)]
    body = []
    for n, ch in enumerate(chunks):
        comma = ',' if n < len(chunks) - 1 else ''
        if n == 0:
            body.append('  [' + ','.join(str(v) for v in ch) + comma)
        else:
            body.append('   ' + ','.join(str(v) for v in ch) + comma)
    body[-1] = body[-1] + '])'
    return [head] + body

converted = 0
kept = []
for ln in lines:
    m = table_re.match(ln)
    if m:
        converted += 1
        kept.extend(emit_table(m.group(1), decode_table(m.group(2))))
    else:
        kept.append(ln)
lines = kept
if converted == 0:
    die('no HappyA# tables found')
sys.stderr.write('mhsify-happy: converted %d tables in %s\n'
                 % (converted, src_path))

# 6. Identifier renames to boxed counterparts.
text = '\n'.join(lines)
text = text.replace('Happy_GHC_Exts.Int#', 'Happy_Prelude.Int')
text = re.sub(r'Happy_GHC_Exts\.I# ', '', text)
text = text.replace('Happy_GHC_Exts.seq', 'Happy_Prelude.seq')

# 7. Strip '#' from decimal literals outside strings/chars/comments.
#    Wrap negative unboxed literals (-1#) in parens ((-1)) so pattern matches
#    such as `happyDecodeAction -1# = ...` do not become infix `(-)`.
#    Haskell-aware scan: skip "..." (with escapes), 'c' literals, -- comments
#    and nested {- -} comments (including {-# pragmas #-}).
num_re = re.compile(r'-?\d+#(?!#)')

def scan(src, rewrite):
    res = []
    state = 'code'
    depth = 0
    k = 0
    n = len(src)
    changed = 0
    while k < n:
        c = src[k]
        if state == 'code':
            if c == '"':
                state = 'str'; res.append(c); k += 1
            elif c == "'":
                nxt1 = src[k + 1] if k + 1 < n else ''
                nxt2 = src[k + 2] if k + 2 < n else ''
                if nxt1 == '\\' or nxt2 == "'":
                    state = 'char'; res.append(c); k += 1
                else:
                    res.append(c); k += 1
            elif c == '-' and k + 1 < n and src[k + 1] == '-':
                state = 'line'; res.append('--'); k += 2
            elif c == '{' and k + 1 < n and src[k + 1] == '-':
                state = 'block'; depth = 1; res.append('{-'); k += 2
            else:
                m = num_re.match(src, k)
                if m and not (res and (res[-1].isalnum() or res[-1] in "_'")):
                    if rewrite:
                        tok = m.group(0)[:-1]
                        res.append('(' + tok + ')' if tok.startswith('-') else tok)
                    else:
                        res.append(m.group(0))
                    changed += 1
                    k += len(m.group(0))
                else:
                    res.append(c); k += 1
        elif state == 'str':
            if c == '\\' and k + 1 < n:
                res.append(src[k:k + 2]); k += 2
            elif c == '"':
                state = 'code'; res.append(c); k += 1
            else:
                res.append(c); k += 1
        elif state == 'char':
            if c == '\\' and k + 1 < n:
                res.append(src[k:k + 2]); k += 2
            elif c == "'":
                state = 'code'; res.append(c); k += 1
            else:
                res.append(c); k += 1
        elif state == 'line':
            res.append(c); k += 1
            if c == '\n':
                state = 'code'
        else:
            if c == '{' and k + 1 < n and src[k + 1] == '-':
                depth += 1; res.append('{-'); k += 2
            elif c == '-' and k + 1 < n and src[k + 1] == '}':
                depth -= 1; res.append('-}'); k += 2
                if depth == 0:
                    state = 'code'
            else:
                res.append(c); k += 1
    return ''.join(res), changed, state, depth

text, n1, st1, d1 = scan(text, True)
if st1 == 'block' or d1 != 0:
    die('unterminated block comment after rewrite')
text2, n2, _, _ = scan(text, False)
if n2 != 0:
    die('%d numeric #-literals remain after rewrite' % n2)
sys.stderr.write('mhsify-happy: stripped %d numeric #-suffixes\n' % n1)

# 8. Final assertions on code regions (strings/comments excluded).
def code_only(src):
    out = []
    state = 'code'
    depth = 0
    k = 0
    n = len(src)
    while k < n:
        c = src[k]
        if state == 'code':
            if c == '"':
                state = 'str'; k += 1
            elif c == "'":
                nxt1 = src[k + 1] if k + 1 < n else ''
                nxt2 = src[k + 2] if k + 2 < n else ''
                if nxt1 == '\\' or nxt2 == "'":
                    state = 'char'; k += 1
                else:
                    out.append(c); k += 1
            elif c == '-' and k + 1 < n and src[k + 1] == '-':
                state = 'line'; k += 2
            elif c == '{' and k + 1 < n and src[k + 1] == '-':
                state = 'block'; depth = 1; k += 2
            else:
                out.append(c); k += 1
        elif state == 'str':
            if c == '\\':
                k += 2
            elif c == '"':
                state = 'code'; k += 1
            else:
                k += 1
        elif state == 'char':
            if c == '\\':
                k += 2
            elif c == "'":
                state = 'code'; k += 1
            else:
                k += 1
        elif state == 'line':
            if c == '\n':
                state = 'code'
            k += 1
        else:
            if c == '{' and k + 1 < n and src[k + 1] == '-':
                depth += 1; k += 2
            elif c == '-' and k + 1 < n and src[k + 1] == '}':
                depth -= 1; k += 2
                if depth == 0:
                    state = 'code'
            else:
                k += 1
    return ''.join(out)

code = code_only(text)
for pat in ('Happy_GHC_Exts', 'HappyA# "', '"#', 'Addr#', 'I#',
            'Int#', 'Word#', 'Char#', 'State#', 'MutVar#',
            'MachDeps.h', 'ghcconfig.h', 'GHC.Num', 'GHC.Err'):
    if pat in code:
        die('leftover %r in generated code' % pat)

with open(dst_path, 'w', encoding='utf-8') as f:
    f.write(text)
sys.stderr.write('mhsify-happy: wrote %s\n' % dst_path)
PYEOF
}

gen_happy() {
  local src="$1" dst="$2"
  if is_fresh "$dst" "$ROOT/$src"; then
    echo "fresh: $dst"
    return 0
  fi
  mkdir -p "$(dirname "$dst")"
  local tmp="$dst.tmp"
  echo "happy $src -> $dst"
  "$HAPPY_BIN" "$ROOT/$src" -o "$tmp"
  mhsify_happy "$tmp" "$dst"
  rm -f "$tmp"
}

gen_alex() {
  local flags="$1" src="$2" dst="$3"
  if is_fresh "$dst" "$ROOT/$src"; then
    echo "fresh: $dst"
    return 0
  fi
  mkdir -p "$(dirname "$dst")"
  local tmp="$dst.tmp"
  echo "alex $flags $src -> $dst"
  # shellcheck disable=SC2086
  "$ALEX_BIN" $flags "$ROOT/$src" -o "$tmp"
  # The template's #include "ghcconfig.h" only serves the (unused here)
  # ALEX_GHC/WORDS_BIGENDIAN branches; cpphs cannot open it, so drop it.
  grep -v 'ghcconfig\.h' "$tmp" > "$dst"
  rm -f "$tmp"
  if grep -q 'ghcconfig.h' "$dst"; then
    echo "gen-parsers.sh: error: ghcconfig.h still present in $dst" >&2
    exit 1
  fi
}

gen_happy "compiler/GHC/Parser.y"        "$OUT_AUTOGEN/GHC/Parser.hs"
gen_happy "compiler/GHC/Cmm/Parser.y"    "$OUT_AUTOGEN/GHC/Cmm/Parser.hs"
gen_happy "utils/genprimopcode/Parser.y" "$OUT_AUTOGEN/genprimopcode/Parser.hs"

gen_alex "--latin1" "compiler/GHC/Parser/Lexer.x"        "$OUT_AUTOGEN/GHC/Parser/Lexer.hs"
gen_alex "--latin1" "compiler/GHC/Parser/Lexer/String.x" "$OUT_AUTOGEN/GHC/Parser/Lexer/String.hs"
gen_alex "--latin1" "compiler/GHC/Parser/HaddockLex.x"   "$OUT_AUTOGEN/GHC/Parser/HaddockLex.hs"
gen_alex "--latin1" "compiler/GHC/Cmm/Lexer.x"           "$OUT_AUTOGEN/GHC/Cmm/Lexer.hs"
gen_alex ""         "utils/genprimopcode/Lexer.x"        "$OUT_AUTOGEN/genprimopcode/Lexer.hs"

echo "gen-parsers.sh: done -> $OUT_AUTOGEN"
