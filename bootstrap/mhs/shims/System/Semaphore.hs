-- MicroHs ships no System.Semaphore package.  This provides the API
-- used by compiler/GHC/Driver/MakeSem.hs (non-wasm branch),
-- compiler/GHC/Driver/MakeAction.hs, compiler/GHC/Driver/Errors/Ppr.hs,
-- compiler/GHC/Driver/Errors/Types.hs and
-- compiler/GHC/Driver/Session.hs, implemented as an in-process
-- single-token semaphore: a one-slot 'MVar' carried by the token, so
-- 'waitOnSemaphore' blocks until 'releaseSemaphoreToken' is called.
-- Stage1 never coordinates with an external jobserver, so local
-- semantics are exact for our purposes.
module System.Semaphore
  ( SemaphoreError(..)
  , SemaphoreIdentifier
  , SemaphoreToken
  , ClientSemaphore
  , SemaphoreProtocolVersion
  , semaphoreVersion
  , getSemaphoreProtocolVersion
  , AbstractSem(..)
  , withAbstractSem
  , openSemaphore
  , waitOnSemaphore
  , releaseSemaphoreToken
  ) where

import Prelude

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception (IOException)
import qualified Control.Monad.Catch as MC

-- | Identifier of a system semaphore (from @-jsem@).  Uninterpreted here.
type SemaphoreIdentifier = String

-- | Protocol version of the semaphore interface.
data SemaphoreProtocolVersion = SemaphoreProtocolVersion Int

-- | The protocol version this shim speaks.
semaphoreVersion :: SemaphoreProtocolVersion
semaphoreVersion = SemaphoreProtocolVersion 1

-- | Project a protocol version to its version number.
getSemaphoreProtocolVersion :: SemaphoreProtocolVersion -> Int
getSemaphoreProtocolVersion (SemaphoreProtocolVersion v) = v

-- | Errors from opening or using a system semaphore.
data SemaphoreError
  = SemaphoreAlreadyExists SemaphoreIdentifier
  | SemaphoreDoesNotExist SemaphoreIdentifier
  | SemaphoreIncompatibleVersion SemaphoreProtocolVersion SemaphoreProtocolVersion
  | SemaphoreOtherError IOException

-- | Handle on an opened (here: in-process) semaphore: a one-slot token box.
newtype ClientSemaphore = ClientSemaphore (MVar ())

-- | A token acquired from a 'ClientSemaphore'; carries the box to release.
data SemaphoreToken = SemaphoreToken (MVar ())

-- | Open the named semaphore.  Always succeeds in-process with a fresh
-- single-token semaphore; the name is uninterpreted.
openSemaphore :: SemaphoreIdentifier -> IO (Either SemaphoreError ClientSemaphore)
openSemaphore _ = fmap (Right . ClientSemaphore) (newMVar ())

-- | Acquire the token, blocking until it is available.
waitOnSemaphore :: ClientSemaphore -> IO SemaphoreToken
waitOnSemaphore (ClientSemaphore mv) = do
  takeMVar mv
  return (SemaphoreToken mv)

-- | Release a previously acquired token.
releaseSemaphoreToken :: SemaphoreToken -> IO ()
releaseSemaphoreToken (SemaphoreToken mv) = putMVar mv ()

-- | A process-local counting semaphore.
data AbstractSem = AbstractSem { acquireSem :: IO (), releaseSem :: IO () }

-- | Run an action while holding the semaphore.
withAbstractSem :: AbstractSem -> IO a -> IO a
withAbstractSem sem = MC.bracket_ (acquireSem sem) (releaseSem sem)
