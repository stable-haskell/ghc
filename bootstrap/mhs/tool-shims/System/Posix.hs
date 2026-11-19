-- Minimal `System.Posix` for the MicroHs stage1 ghc-pkg build.
--
-- MicroHs `base` has no `System.Posix` at all, so this module is found
-- without any shadowing problem. Only what ghc-pkg uses is provided:
-- signal installation for Ctrl-C handling.
--
-- Limitation: asynchronous delivery of a caught signal to a Haskell
-- thread is unavailable (the MicroHs runtime cannot run Haskell code
-- from a C signal handler), so `installHandler` accepts and discards
-- the handler, keeps the default disposition, and reports `Default`
-- as the previous handler. Interrupting ghc-pkg therefore terminates
-- it instead of running its cleanup action.
module System.Posix
  ( Handler(..)
  , Signal
  , sigINT
  , sigQUIT
  , installHandler
  ) where

import Prelude

newtype Signal = Signal Int
  deriving (Eq, Show)

sigINT :: Signal
sigINT = Signal 2

sigQUIT :: Signal
sigQUIT = Signal 3

data Handler
  = Default
  | Ignore
  | Catch (IO ())

installHandler :: Signal -> Handler -> Maybe Handler -> IO Handler
installHandler _ _ _ = return Default
