{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, UnliftedFFITypes,
             GHCForeignImportPrim, DeriveDataTypeable, GHCForeignImportPrim #-}
module GHCJS.Foreign.Callback
    ( Callback
    , OnBlocked(..)
    , releaseCallback
    , asyncCallback
    , asyncCallback1
    , asyncCallback2
    , syncCallback
    , syncCallback1
    , syncCallback2
    ) where

--import           GHCJS.Marshal
--import           GHCJS.Marshal.Pure
import           GHCJS.Foreign.Callback.Internal
import           GHCJS.Prim
import           GHCJS.Types

import qualified GHC.Exts as Exts

import           Data.Typeable

import           Unsafe.Coerce

{- |
     The runtime tries to run synchronous threads to completion. Sometimes it's
     not possible to continue running a thread, for example when the thread
     tries to take an empty 'MVar'. The runtime can then either throw a
     'WouldBlockException', aborting the blocking action, or continue the
     thread asynchronously.
 -}
data OnBlocked = ContinueAsync   -- ^ continue the thread asynchronously if blocked
               | ThrowWouldBlock -- ^ throw 'WouldBlockException' if blocked
               deriving (Show, Eq, Enum, Typeable)

{- |
     When you create a callback, the Haskell runtime stores a reference to
     the exported IO action or function. This means that all data referenced by the
     exported value stays in memory, even if nothing outside the Haskell runtime
     holds a reference to to callback.

     Use 'releaseCallback' to free the reference. Subsequent calls from JavaScript
     to the callback will result in an exception.
 -}
releaseCallback :: Callback a -> IO ()
releaseCallback x = error "releaseCallback: only available in JavaScript"

{- | Make a callback (JavaScript function) that runs the supplied IO action in a synchronous
     thread when called.

     Call 'releaseCallback' when done with the callback, freeing memory referenced
     by the IO action.
 -}
syncCallback :: OnBlocked                               -- ^ what to do when the thread blocks
             -> IO ()                                   -- ^ the Haskell action
             -> IO (Callback (IO ()))                   -- ^ the callback
syncCallback onBlocked x = error "syncCallback: only available in JavaScript"


{- | Make a callback (JavaScript function) that runs the supplied IO function in a synchronous
     thread when called. The callback takes one argument that it passes as a JSRef value to
     the Haskell function.

     Call 'releaseCallback' when done with the callback, freeing data referenced
     by the function.
 -}
syncCallback1 :: OnBlocked                               -- ^ what to do when the thread blocks
              -> (JSRef a -> IO ())                      -- ^ the Haskell function
              -> IO (Callback (JSRef a -> IO ()))        -- ^ the callback
syncCallback1 onBlocked x = error "syncCallback1: only available in JavaScript"


{- | Make a callback (JavaScript function) that runs the supplied IO function in a synchronous
     thread when called. The callback takes two arguments that it passes as JSRef values to
     the Haskell function.

     Call 'releaseCallback' when done with the callback, freeing data referenced
     by the function.
 -}
syncCallback2 :: OnBlocked                                   -- ^ what to do when the thread blocks
              -> (JSRef a -> JSRef b -> IO ())               -- ^ the Haskell function
              -> IO (Callback (JSRef a -> JSRef b -> IO ())) -- ^ the callback
syncCallback2 onBlocked x = error "syncCallback2: only available in JavaScript"


{- | Make a callback (JavaScript function) that runs the supplied IO action in an asynchronous
     thread when called.

     Call 'releaseCallback' when done with the callback, freeing data referenced
     by the IO action.
 -}
asyncCallback :: IO ()              -- ^ the action that the callback runs
              -> IO (Callback (IO ())) -- ^ the callback
asyncCallback x = error "asyncCallback: only available in JavaScript"

asyncCallback1 :: (JSRef a -> IO ())            -- ^ the function that the callback calls
               -> IO (Callback (JSRef a -> IO ())) -- ^ the calback
asyncCallback1 x = error "asyncCallback1: only available in JavaScript"

asyncCallback2 :: (JSRef a -> JSRef b -> IO ())            -- ^ the Haskell function that the callback calls
               -> IO (Callback (JSRef a -> JSRef b -> IO ())) -- ^ the callback
asyncCallback2 x = error "asyncCallback2: only available in JavaScript"

-- ----------------------------------------------------------------------------
