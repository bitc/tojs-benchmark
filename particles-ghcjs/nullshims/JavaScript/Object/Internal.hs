{-# LANGUAGE DeriveDataTypeable, ForeignFunctionInterface, JavaScriptFFI,
             UnboxedTuples, GHCForeignImportPrim, EmptyDataDecls, UnliftedFFITypes
  #-}

module JavaScript.Object.Internal
    ( Object(..)
    , create
    , allProps
    , listProps
    , getProp
    , unsafeGetProp
    , setProp
    , unsafeSetProp
    , isInstanceOf
    ) where

import           Data.JSString
import           Data.Typeable

import qualified GHCJS.Prim                as Prim
import           GHCJS.Types

import qualified JavaScript.Array          as JA
import           JavaScript.Array.Internal (JSArray, SomeJSArray(..))

newtype Object = Object (JSRef ()) deriving (Typeable)

-- | create an empty object
create :: IO Object
create = js_create
{-# INLINE create #-}

allProps :: Object -> IO JSArray
allProps o = js_allProps o
{-# INLINE allProps #-}

listProps :: Object -> IO [JSString]
listProps o = case js_listProps o of (# ps #) -> return ps
{-# INLINE listProps #-}

{- | get a property from an object. If accessing the property results in
     an exception, the exception is converted to a JSException. Since exception
     handling code prevents some optimizations in some JS engines, you may want
     to use unsafeGetProp instead
 -}
getProp :: JSString -> Object -> IO (JSRef a)
getProp p o = js_getProp p o
{-# INLINE getProp #-}

unsafeGetProp :: JSString -> Object -> IO (JSRef a)
unsafeGetProp p o = js_unsafeGetProp p o
{-# INLINE unsafeGetProp #-}

setProp :: JSString -> JSRef a -> Object -> IO ()
setProp p v o = js_setProp p v o
{-# INLINE setProp #-}

unsafeSetProp :: JSString -> JSRef a -> Object -> IO ()
unsafeSetProp p v o = js_unsafeSetProp p v o
{-# INLINE unsafeSetProp #-}

isInstanceOf :: Object -> JSRef a -> Bool
isInstanceOf o s = js_isInstanceOf o s
{-# INLINE isInstanceOf #-}

-- -----------------------------------------------------------------------------

js_create        :: IO Object
js_create        = error "js_create       : only available in JavaScript"
js_getProp       :: JSString -> Object -> IO (JSRef b)
js_getProp       = error "js_getProp      : only available in JavaScript"
js_unsafeGetProp :: JSString -> Object -> IO (JSRef b)
js_unsafeGetProp = error "js_unsafeGetProp: only available in JavaScript"
js_setProp       :: JSString -> JSRef a -> Object -> IO ()
js_setProp       = error "js_setProp      : only available in JavaScript"
js_unsafeSetProp :: JSString -> JSRef a -> Object -> IO ()
js_unsafeSetProp = error "js_unsafeSetProp: only available in JavaScript"
js_isInstanceOf  :: Object -> JSRef a -> Bool
js_isInstanceOf  = error "js_isInstanceOf : only available in JavaScript"
js_allProps      :: Object -> IO JSArray
js_allProps      = error "js_allProps     : only available in JavaScript"
js_listProps     :: Object -> (# [JSString] #)
js_listProps     = error "js_listProps: only available in JavaScript"
