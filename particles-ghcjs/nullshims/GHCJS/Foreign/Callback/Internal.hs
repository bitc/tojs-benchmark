module GHCJS.Foreign.Callback.Internal where

import GHCJS.Types
--import GHCJS.Marshal.Internal

newtype Callback a = Callback (JSRef ())
