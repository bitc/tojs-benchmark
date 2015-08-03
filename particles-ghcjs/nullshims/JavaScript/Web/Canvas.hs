{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, JavaScriptFFI,
             OverloadedStrings, DeriveDataTypeable
  #-}

module JavaScript.Web.Canvas ( Context
                             , Canvas
                             , Image
                             , TextAlign(..)
                             , TextBaseline(..)
                             , LineCap(..)
                             , LineJoin(..)
                             , Repeat(..)
                             , Gradient
                             , Pattern
                             , create
                             , getContext
                             , save
                             , restore
                             , scale
                             , rotate
                             , translate
                             , transform
                             , setTransform
                             , fill
                             , fillRule
                             , stroke
                             , beginPath
                             , closePath
                             , clip
                             , moveTo
                             , lineTo
                             , quadraticCurveTo
                             , bezierCurveTo
                             , arc
                             , arcTo
                             , rect
                             , isPointInPath
                             , fillStyle
                             , strokeStyle
                             , globalAlpha
                             , lineJoin
                             , lineCap
                             , lineWidth
                             , setLineDash
                             , lineDashOffset
                             , miterLimit
                             , fillText
                             , strokeText
                             , font
                             , measureText
                             , textAlign
                             , textBaseline
                             , fillRect
                             , strokeRect
                             , clearRect
                             , drawImage
                             ) where

import Prelude hiding (Left, Right)

import Control.Applicative
import Control.Monad

import Data.Data
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Typeable

--import GHCJS.Foreign
--import GHCJS.Marshal
import GHCJS.Types

import           JavaScript.Web.Canvas.Internal

import           JavaScript.Object (Object)
import qualified JavaScript.Object as O
import           JavaScript.Array  (JSArray)
import qualified JavaScript.Array  as A

data TextAlign = Start
               | End
               | Left
               | Right
               | Center
             deriving (Eq, Show, Enum, Data, Typeable)

data TextBaseline = Top 
                  | Hanging 
                  | Middle
                  | Alphabetic
                  | Ideographic
                  | Bottom
                deriving (Eq, Show, Enum, Data, Typeable)

data LineJoin = LineJoinBevel
              | LineJoinRound
              | LineJoinMiter
            deriving (Eq, Show, Enum)

data LineCap = LineCapButt
             | LineCapRound
             | LineCapSquare deriving (Eq, Show, Enum, Data, Typeable)

data Repeat = Repeat
            | RepeatX
            | RepeatY
            | NoRepeat
            deriving (Eq, Ord, Show, Enum, Data, Typeable)

unsafeToCanvas :: JSRef () -> Canvas
unsafeToCanvas r = error "unsafeToCanvas: only available in JavaScript"
{-# INLINE unsafeToCanvas #-}

toCanvas :: JSRef () -> Maybe Canvas
toCanvas x = error "toCanvas" -- fixme
{-# INLINE toCanvas #-}

create :: Int -> Int -> IO Canvas
create = js_create
{-# INLINE create #-}

getContext :: Canvas -> IO Context
getContext c = js_getContext c
{-# INLINE getContext #-}

save :: Context -> IO ()
save ctx = js_save ctx
{-# INLINE save #-}

restore :: Context -> IO ()
restore = js_restore
{-# INLINE restore #-}

transform :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
transform = js_transform
{-# INLINE transform #-}

setTransform :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
setTransform = js_setTransform
{-# INLINE setTransform #-}

scale :: Double -> Double -> Context -> IO ()
scale x y ctx = js_scale x y ctx
{-# INLINE scale #-}

translate :: Double -> Double -> Context -> IO ()
translate x y ctx = js_translate x y ctx
{-# INLINE translate #-}

rotate :: Double -> Context -> IO ()
rotate r ctx = js_rotate r ctx
{-# INLINE rotate #-}

fill :: Context -> IO ()
fill ctx = js_fill ctx
{-# INLINE fill #-}

fillRule :: JSString -> Context -> IO ()
fillRule rule ctx = js_fill_rule rule ctx
{-# INLINE fillRule #-}

stroke :: Context -> IO ()
stroke = js_stroke
{-# INLINE stroke #-}

beginPath :: Context -> IO ()
beginPath = js_beginPath
{-# INLINE beginPath #-}

closePath :: Context -> IO ()
closePath = js_closePath
{-# INLINE closePath #-}

clip :: Context -> IO ()
clip = js_clip
{-# INLINE clip #-}

moveTo :: Double -> Double -> Context -> IO ()
moveTo = js_moveTo
{-# INLINE moveTo #-}

lineTo :: Double -> Double -> Context -> IO ()
lineTo = js_lineTo
{-# INLINE lineTo #-}

quadraticCurveTo :: Double -> Double -> Double -> Double -> Context -> IO ()
quadraticCurveTo = js_quadraticCurveTo
{-# INLINE quadraticCurveTo #-}

bezierCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
bezierCurveTo = js_bezierCurveTo
{-# INLINE bezierCurveTo #-}

arc :: Double -> Double -> Double -> Double -> Double -> Bool -> Context -> IO ()
arc a b c d e bl ctx = js_arc a b c d e bl ctx
{-# INLINE arc #-}

arcTo :: Double -> Double -> Double -> Double -> Double -> Context -> IO ()
arcTo = js_arcTo
{-# INLINE arcTo #-}

rect :: Double -> Double -> Double -> Double -> Context -> IO ()
rect = js_rect
{-# INLINE rect #-}

isPointInPath :: Double -> Double -> Context -> IO ()
isPointInPath = js_isPointInPath
{-# INLINE isPointInPath #-}

fillStyle :: Int -> Int -> Int -> Double -> Context -> IO ()
fillStyle = js_fillStyle
{-# INLINE fillStyle #-}

strokeStyle :: Int -> Int -> Int -> Double -> Context -> IO ()
strokeStyle = js_strokeStyle
{-# INLINE strokeStyle #-}

globalAlpha :: Double -> Context -> IO ()
globalAlpha = js_globalAlpha
{-# INLINE globalAlpha #-}

lineJoin :: LineJoin -> Context -> IO ()
lineJoin LineJoinBevel ctx = js_lineJoin "bevel" ctx
lineJoin LineJoinRound ctx = js_lineJoin "round" ctx
lineJoin LineJoinMiter ctx = js_lineJoin "miter" ctx
{-# INLINE lineJoin #-}

lineCap :: LineCap -> Context -> IO ()
lineCap LineCapButt   ctx = js_lineCap "butt"   ctx
lineCap LineCapRound  ctx = js_lineCap "round"  ctx
lineCap LineCapSquare ctx = js_lineCap "square" ctx
{-# INLINE lineCap #-}

miterLimit :: Double -> Context -> IO ()
miterLimit = js_miterLimit
{-# INLINE miterLimit #-}

-- | pass an array of numbers
setLineDash :: JSArray -> Context -> IO ()
setLineDash arr ctx = js_setLineDash arr ctx
{-# INLINE setLineDash #-}

lineDashOffset :: Double -> Context -> IO ()
lineDashOffset = js_lineDashOffset
{-# INLINE lineDashOffset #-}

textAlign :: TextAlign -> Context -> IO ()
textAlign align ctx = case align of
     Start  -> js_textAlign "start"  ctx
     End    -> js_textAlign "end"    ctx
     Left   -> js_textAlign "left"   ctx
     Right  -> js_textAlign "right"  ctx
     Center -> js_textAlign "center" ctx
{-# INLINE textAlign #-}

textBaseline :: TextBaseline -> Context -> IO ()
textBaseline baseline ctx = case baseline of 
     Top         -> js_textBaseline "top"         ctx
     Hanging     -> js_textBaseline "hanging"     ctx
     Middle      -> js_textBaseline "middle"      ctx
     Alphabetic  -> js_textBaseline "alphabetic"  ctx
     Ideographic -> js_textBaseline "ideographic" ctx
     Bottom      -> js_textBaseline "bottom"      ctx
{-# INLINE textBaseline #-}

lineWidth :: Double -> Context -> IO ()
lineWidth = js_lineWidth
{-# INLINE lineWidth #-}

fillText :: JSString -> Double -> Double -> Context -> IO ()
fillText t x y ctx = js_fillText t x y ctx
{-# INLINE fillText #-}

strokeText :: JSString -> Double -> Double -> Context -> IO ()
strokeText t x y ctx = js_strokeText t x y ctx
{-# INLINE strokeText #-}

font :: JSString -> Context -> IO ()
font f ctx = js_font f ctx
{-# INLINE font #-}

measureText :: JSString -> Context -> IO Double
measureText = error "measureText: only available in JavaScript"
{-# INLINE measureText #-}

fillRect :: Double -> Double -> Double -> Double -> Context -> IO ()
fillRect = js_fillRect
{-# INLINE fillRect #-}

clearRect :: Double -> Double -> Double -> Double -> Context -> IO ()
clearRect = js_clearRect
{-# INLINE clearRect #-}

strokeRect :: Double -> Double -> Double -> Double -> Context -> IO ()
strokeRect = js_strokeRect
{-# INLINE strokeRect #-}

drawImage :: Image -> Int -> Int -> Int -> Int -> Context -> IO ()
drawImage = js_drawImage
{-# INLINE drawImage #-}

createPattern :: Image -> Repeat -> Context -> IO Pattern
createPattern img Repeat   ctx = js_createPattern img "repeat"    ctx
createPattern img RepeatX  ctx = js_createPattern img "repeat-x"  ctx
createPattern img RepeatY  ctx = js_createPattern img "repeat-y"  ctx
createPattern img NoRepeat ctx = js_createPattern img "no-repeat" ctx
{-# INLINE createPattern #-}

setWidth :: Int -> Canvas -> IO ()
setWidth w c = js_setWidth w c
{-# INLINE setWidth #-}

width :: Canvas -> IO Int
width c = js_width c
{-# INLINE width #-}

setHeight :: Int -> Canvas -> IO ()
setHeight h c = js_setHeight h c
{-# INLINE setHeight #-}

height :: Canvas -> IO Int
height c = js_height c
{-# INLINE height #-}

-- ----------------------------------------------------------------------------

js_create :: Int -> Int -> IO Canvas
js_create = error "js_create: only available in JavaScript"
js_getContext :: Canvas -> IO Context
js_getContext = error "js_getContext: only available in JavaScript"
js_save :: Context -> IO ()
js_save = error "js_save: only available in JavaScript"
js_restore  :: Context -> IO ()
js_restore  = error "js_restore : only available in JavaScript"
js_transform :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
js_transform = error "js_transform: only available in JavaScript"
js_setTransform :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
js_setTransform = error "js_setTransform: only available in JavaScript"
js_scale :: Double -> Double -> Context -> IO ()
js_scale = error "js_scale: only available in JavaScript"
js_translate  :: Double -> Double -> Context -> IO ()
js_translate  = error "js_translate : only available in JavaScript"
js_rotate :: Double -> Context -> IO ()
js_rotate = error "js_rotate: only available in JavaScript"
js_fill :: Context -> IO ()
js_fill = error "js_fill: only available in JavaScript"
js_fill_rule  :: JSString -> Context -> IO ()
js_fill_rule  = error "js_fill_rule : only available in JavaScript"
js_stroke :: Context -> IO ()
js_stroke = error "js_stroke: only available in JavaScript"
js_beginPath :: Context -> IO ()
js_beginPath = error "js_beginPath: only available in JavaScript"
js_closePath :: Context -> IO ()
js_closePath = error "js_closePath: only available in JavaScript"
js_clip  :: Context -> IO ()
js_clip  = error "js_clip : only available in JavaScript"
js_moveTo :: Double -> Double -> Context -> IO ()
js_moveTo = error "js_moveTo: only available in JavaScript"
js_lineTo :: Double -> Double -> Context -> IO ()
js_lineTo = error "js_lineTo: only available in JavaScript"
js_quadraticCurveTo :: Double -> Double -> Double -> Double -> Context -> IO ()
js_quadraticCurveTo = error "js_quadraticCurveTo: only available in JavaScript"
js_bezierCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
js_bezierCurveTo = error "js_bezierCurveTo: only available in JavaScript"
js_arc :: Double -> Double -> Double -> Double -> Double -> Bool -> Context -> IO ()
js_arc = error "js_arc: only available in JavaScript"
js_arcTo :: Double -> Double -> Double -> Double -> Double -> Context -> IO ()
js_arcTo = error "js_arcTo: only available in JavaScript"
js_rect :: Double -> Double -> Double -> Double -> Context -> IO ()
js_rect = error "js_rect: only available in JavaScript"
js_isPointInPath :: Double -> Double -> Context -> IO ()
js_isPointInPath = error "js_isPointInPath: only available in JavaScript"
js_fillStyle :: Int -> Int -> Int -> Double -> Context -> IO ()
js_fillStyle = error "js_fillStyle: only available in JavaScript"
js_strokeStyle :: Int -> Int -> Int -> Double -> Context -> IO ()
js_strokeStyle = error "js_strokeStyle: only available in JavaScript"
js_globalAlpha :: Double           -> Context -> IO ()
js_globalAlpha = error "js_globalAlpha: only available in JavaScript"
js_lineJoin :: JSString -> Context -> IO ()
js_lineJoin = error "js_lineJoin: only available in JavaScript"
js_lineCap :: JSString -> Context -> IO ()
js_lineCap = error "js_lineCap: only available in JavaScript"
js_miterLimit :: Double -> Context -> IO ()
js_miterLimit = error "js_miterLimit: only available in JavaScript"
js_setLineDash :: JSArray -> Context -> IO ()
js_setLineDash = error "js_setLineDash: only available in JavaScript"
js_lineDashOffset :: Double -> Context -> IO ()
js_lineDashOffset = error "js_lineDashOffset: only available in JavaScript"
js_font :: JSString -> Context -> IO ()
js_font = error "js_font: only available in JavaScript"
js_textAlign :: JSString -> Context -> IO ()
js_textAlign = error "js_textAlign: only available in JavaScript"
js_textBaseline :: JSString -> Context -> IO ()
js_textBaseline = error "js_textBaseline: only available in JavaScript"
js_lineWidth :: Double -> Context -> IO ()
js_lineWidth = error "js_lineWidth: only available in JavaScript"
js_fillText :: JSString -> Double -> Double -> Context -> IO ()
js_fillText = error "js_fillText: only available in JavaScript"
js_strokeText :: JSString -> Double -> Double -> Context -> IO ()
js_strokeText = error "js_strokeText: only available in JavaScript"
js_measureText :: JSString                    -> Context -> IO Object
js_measureText = error "js_measureText: only available in JavaScript"
js_fillRect :: Double -> Double -> Double -> Double -> Context -> IO ()
js_fillRect = error "js_fillRect: only available in JavaScript"
js_clearRect :: Double -> Double -> Double -> Double -> Context -> IO ()
js_clearRect = error "js_clearRect: only available in JavaScript"
js_strokeRect :: Double -> Double -> Double -> Double -> Context -> IO ()
js_strokeRect = error "js_strokeRect: only available in JavaScript"
js_drawImage :: Image -> Int -> Int -> Int -> Int -> Context -> IO () 
js_drawImage = error "js_drawImage: only available in JavaScript"
js_createPattern :: Image -> JSString -> Context -> IO Pattern
js_createPattern = error "js_createPattern: only available in JavaScript"
js_width :: Canvas -> IO Int
js_width = error "js_width: only available in JavaScript"
js_height :: Canvas -> IO Int
js_height = error "js_height: only available in JavaScript"
js_setWidth :: Int -> Canvas -> IO ()
js_setWidth = error "js_setWidth: only available in JavaScript"
js_setHeight :: Int -> Canvas -> IO ()
js_setHeight = error "js_setHeight: only available in JavaScript"
