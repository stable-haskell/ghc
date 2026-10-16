{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM_, unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import GHC.SysTools.Ar

-- Exercise the public archive reader, including member sizes and the cursor
-- for the following member. Apple ranlib uses BSD extended names.
main :: IO ()
main = do
  forM_ [0 .. 7] $ \padding -> do
    forM_ [header, custom, largeCustom, longLength] $ \wasm -> do
      checkBSD ("padding " ++ show padding) (wasm <> B.replicate padding 10) wasm
  forM_ malformed $ \bytes -> checkBSD "malformed member" bytes bytes
  checkBSD "other object format" "ELF\n\n" "ELF\n\n"
  checkBSD "unknown wasm version" "\0asm\2\0\0\0\n" "\0asm\2\0\0\0\n"
  checkBSD "excess padding" (header <> B.replicate 8 10)
                            (header <> B.replicate 8 10)
  -- The GNU reader must not change an archive member's payload.
  let padded = custom <> "\n\n"
  check "GNU archive" (readMembers (member "x.o/" padded)) [padded]
  putStrLn "OK"
  where
    header = "\0asm\1\0\0\0"
    -- A custom section named x with a newline in its payload.
    custom = header <> B.pack [0, 3, 1, 120, 10]
    largeCustom = header <> B.pack [0, 0x82, 1, 0] <> B.replicate 129 10
    -- Valid, non-minimal five-byte encoding of a one-byte section length.
    longLength = header <> B.pack [0, 0x81, 0x80, 0x80, 0x80, 0, 0]
    malformed = map (header <>)
      [ B.pack [0, 0x80]                         -- truncated length
      , B.pack [0, 3, 0]                         -- truncated payload
      , B.pack [0, 0x80, 0x80, 0x80, 0x80, 0x10] -- exceeds u32
      , B.pack [0, 0x80, 0x80, 0x80, 0x80, 0x80, 0] -- too many bytes
      , B.pack [0, 0xff, 0xff, 0xff, 0xff, 0x0f] -- length exceeds member
      , B.pack (0 : replicate 9 0x80 ++ [1])      -- used to overflow Int
      , B.drop 8 custom <> B.pack [0, 0x80]      -- malformed after valid section
      ]
      ++ ["\0asm", "\0asm\1\0\0"]

checkBSD :: String -> B.ByteString -> B.ByteString -> IO ()
checkBSD label input expected = do
  let name = "long-wasm-object-name.o\0\0"
      archive = member ("#1/" ++ show (B.length name)) (name <> input)
             <> member "after.o" "ELF\n"
  check label (readMembers archive) [expected, "ELF\n"]

readMembers :: B.ByteString -> [B.ByteString]
readMembers bytes = case parseAr ("!<arch>\n" <> bytes) of
  Archive entries -> map checkedPayload entries
  where
    checkedPayload entry
      | filesize entry == B.length (filedata entry) = filedata entry
      | otherwise = error "archive member size does not match its payload"

member :: String -> B.ByteString -> B.ByteString
member name bytes = C.pack (field 16 name ++ field 12 "0" ++ field 6 "0"
                        ++ field 6 "0" ++ field 8 "644"
                        ++ field 10 (show (B.length bytes)) ++ "`\n")
                 <> bytes <> if odd (B.length bytes) then "\n" else ""
  where
    field width value = take width (value ++ repeat ' ')

check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label actual expected = unless (actual == expected) $
  error (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
