{-# LANGUAGE MagicHash, DeriveDataTypeable, CPP #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif

module GHCJS.Prim ( JSRef(..)
                  , JSException(..)
                  , WouldBlockException(..)
                  , mkJSException
                  , fromJSString
                  , toJSString
                  , toJSArray
                  , fromJSArray
                  , fromJSInt
                  , toJSInt
                  , isNull
                  , isUndefined
                  , jsNull
                  , getProp
                  , getProp'
                  , eqRef
                  ) where

import           Data.Typeable (Typeable)
import           Unsafe.Coerce (unsafeCoerce)

import           GHC.Prim
import qualified GHC.Exception as Ex

{-
  JSRef is a boxed type that can be used as FFI
  argument or result.
-}
#ifdef ghcjs_HOST_OS
data JSRef a = JSRef ByteArray#
#else
data JSRef a = JSRef Addr#
#endif

{-
  When a JavaScript exception is raised inside
  a safe or interruptible foreign call, it is converted
  to a JSException
 -}
data JSException = JSException (JSRef ()) String
  deriving (Typeable)

instance Ex.Exception JSException

instance Show JSException where
  show (JSException _ xs) = "JavaScript exception: " ++ xs

mkJSException :: JSRef a -> IO JSException
mkJSException ref =
  return (JSException (unsafeCoerce ref) (fromJSString ref))

{- | Low-level conversion utilities for packages that cannot
     depend on ghcjs-base
 -}

{- | returns an empty string if the JSRef does not contain
     a string
 -}
fromJSString :: JSRef a -> String
fromJSString = unsafeCoerce . js_fromJSString
{-# INLINE fromJSString #-}

toJSString :: String -> JSRef a
toJSString = js_toJSString . unsafeCoerce . seqList
{-# INLINE toJSString #-}

fromJSArray :: JSRef a -> IO [JSRef a]
fromJSArray = unsafeCoerce . js_fromJSArray
{-# INLINE fromJSArray #-}

toJSArray :: [JSRef a] -> IO (JSRef b)
toJSArray = js_toJSArray . unsafeCoerce . seqList
{-# INLINE toJSArray #-}

{- | returns zero if the JSRef does not contain a number
 -}
fromJSInt :: JSRef a -> Int
fromJSInt = js_fromJSInt
{-# INLINE fromJSInt #-}

toJSInt :: Int -> JSRef a
toJSInt = js_toJSInt
{-# INLINE toJSInt #-}

isNull :: JSRef a -> Bool
isNull = js_isNull
{-# INLINE isNull #-}

isUndefined :: JSRef a -> Bool
isUndefined = js_isUndefined
{-# INLINE isUndefined #-}

jsNull :: JSRef a
jsNull = js_null
{-# INLINE jsNull #-}

eqRef :: JSRef a -> JSRef b -> Bool
eqRef x y = js_eqRef x y
{-# INLINE eqRef #-}

instance Eq (JSRef a) where
  (==) = eqRef

getProp :: JSRef a -> String -> IO (JSRef b)
getProp o p = js_getProp o (unsafeCoerce $ seqList p)
{-# INLINE getProp #-}

getProp' :: JSRef a -> JSRef b -> IO (JSRef c)
getProp' o p = js_getProp' o p
{-# INLINE getProp' #-}

-- reduce the spine and all list elements to whnf
seqList :: [a] -> [a]
seqList xs = go xs `seq` xs
  where go (x:xs) = x `seq` go xs
        go []     = ()

seqListSpine :: [a] -> [a]
seqListSpine xs = go xs `seq` xs
  where go (x:xs) = go xs
        go []     = ()

js_fromJSString :: JSRef a -> Double
js_fromJSString = error "js_fromJSString: only available in JavaScript"

js_toJSString :: Double -> JSRef a
js_toJSString = error "js_toJSString: only available in JavaScript"

js_fromJSArray :: JSRef a -> IO Double
js_fromJSArray = error "js_fromJSArray: only available in JavaScript"

js_toJSArray :: Double -> IO (JSRef a)
js_toJSArray = error "js_toJSArray: only available in JavaScript"

js_isNull :: JSRef a -> Bool
js_isNull = error "js_isNull: only available in JavaScript"

js_isUndefined :: JSRef a -> Bool
js_isUndefined = error "js_isUndefined: only available in JavaScript"

js_fromJSInt :: JSRef a -> Int
js_fromJSInt = error "js_fromJSInt: only available in JavaScript"
js_toJSInt :: Int -> JSRef a
js_toJSInt = error "js_toJSInt: only available in JavaScript"
js_null :: JSRef a
js_null = error "js_null: only available in JavaScript"
js_eqRef :: JSRef a -> JSRef b -> Bool
js_eqRef = error "js_eqRef: only available in JavaScript"
js_getProp :: JSRef a -> Double -> IO (JSRef b)
js_getProp = error "js_getProp: only available in JavaScript"
js_getProp' :: JSRef a -> JSRef b -> IO (JSRef c)
js_getProp' = error "js_getProp': only available in JavaScript"
{- | If a synchronous thread tries to do something that can only
     be done asynchronously, and the thread is set up to not
     continue asynchronously, it receives this exception.
 -}
data WouldBlockException = WouldBlockException String
  deriving (Show, Typeable)
instance Ex.Exception WouldBlockException
