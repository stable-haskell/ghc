-- MicroHs has no GHC.Show.  Stage1 pretty-printing only needs
-- showMultiLineString.  The bodies match GHC.Internal.Show so Hs
-- string literals keep GHC's escape and line-break shape.
module GHC.Show (showMultiLineString) where

import Prelude

showMultiLineString :: String -> [String]
showMultiLineString str = go '\"' str
  where
    go ch s = case break (== '\n') s of
                (l, _:s'@(_:_)) -> (ch : showLitString l "\\n\\") : go '\\' s'
                (l, "\n")       -> [ch : showLitString l "\\n\""]
                (l, _)          -> [ch : showLitString l "\""]

showLitString :: String -> ShowS
showLitString []         s = s
showLitString ('"' : cs) s = showString "\\\"" (showLitString cs s)
showLitString (c   : cs) s = showLitChar c (showLitString cs s)

showLitChar :: Char -> ShowS
showLitChar c s | c > '\DEL' = showChar '\\' (protectEsc isDec (shows (fromEnum c)) s)
showLitChar '\DEL'   s = showString "\\DEL" s
showLitChar '\\'     s = showString "\\\\" s
showLitChar c s | c >= ' ' = showChar c s
showLitChar '\a'     s = showString "\\a" s
showLitChar '\b'     s = showString "\\b" s
showLitChar '\f'     s = showString "\\f" s
showLitChar '\n'     s = showString "\\n" s
showLitChar '\r'     s = showString "\\r" s
showLitChar '\t'     s = showString "\\t" s
showLitChar '\v'     s = showString "\\v" s
showLitChar '\SO'    s = protectEsc (== 'H') (showString "\\SO") s
showLitChar c        s = showString ('\\' : asciiTab !! fromEnum c) s

showChar :: Char -> ShowS
showChar = (:)

showString :: String -> ShowS
showString = (++)

isDec :: Char -> Bool
isDec c = c >= '0' && c <= '9'

protectEsc :: (Char -> Bool) -> ShowS -> ShowS
protectEsc p f = f . cont
  where
    cont s@(c:_) | p c = "\\&" ++ s
    cont s             = s

asciiTab :: [String]
asciiTab =
  ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
   "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
   "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
   "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
   "SP"]
