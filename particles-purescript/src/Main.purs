module Main where

import Control.Monad.Eff
import Data.Foldable
import Data.Int (toNumber)
import Math
import Prelude
import qualified Graphics.Canvas as C

import Barrier
import Particle
import World

main = do
    canvas <- createCanvas 640 480
    addCanvasToBody canvas

    ctx <- C.getContext2D canvas

    numBarriers <- js_getNumBarriers
    numParticles <- js_getNumParticles

    let world = newWorld numBarriers numParticles

    startTime <- js_now

    numFrames <- js_getNumFrames
    animate startTime numFrames ctx world

animate :: forall eff. Number -> Int -> C.Context2D -> World -> Eff (canvas :: C.Canvas | eff) Unit
animate startTime framesLeft ctx world = do
    renderWorld ctx world
    let next = stepWorld world
    if framesLeft == 0
        then do
            endTime <- js_now
            js_alertNumber (endTime - startTime)
        else do
            js_requestAnimationFrame (animate startTime (framesLeft - 1) ctx next)

renderWorld :: forall eff. C.Context2D -> World -> Eff (canvas :: C.Canvas | eff) Unit
renderWorld ctx world = do
    C.clearRect ctx
        { x: 0.0
        , y: 0.0
        , w: 640.0
        , h: 480.0
        }
    traverse_ renderBarrier world.barriers
    traverse_ renderParticle world.particles
    where
    renderParticle :: forall eff. Particle -> Eff (canvas :: C.Canvas | eff) Unit
    renderParticle p = do
        C.beginPath ctx
        C.arc ctx
            { x: (toNumber p.posX) / 1000.0
            , y: (toNumber p.posY) / 1000.0
            , r: (toNumber p.radius) / 1000.0
            , start: 0.0
            , end: 2.0 * pi
            }
        C.setFillStyle ("rgba(" <> show 0 <> "," <> show p.color <> "," <> show 0 <> "," <> show 1.0 <> ")") ctx
        C.fill ctx
        return unit
    renderBarrier :: forall eff. Barrier -> Eff (canvas :: C.Canvas | eff) Unit
    renderBarrier b = do
        C.beginPath ctx
        C.arc ctx
            { x: (toNumber b.barrierX) / 1000.0
            , y: (toNumber b.barrierY) / 1000.0
            , r: (toNumber b.barrierRadius) / 1000.0
            , start: 0.0
            , end: 2.0 * pi
            }
        C.setFillStyle ("rgba(" <> show 128 <> "," <> show 128 <> "," <> show 128 <> "," <> show 1.0 <> ")") ctx
        C.fill ctx
        return unit


foreign import createCanvas :: forall eff. Int -> Int -> Eff (canvas :: C.Canvas | eff) C.CanvasElement
foreign import addCanvasToBody :: forall eff. C.CanvasElement -> Eff (canvas :: C.Canvas | eff) Unit
foreign import js_now :: forall eff. Eff (canvas :: C.Canvas | eff) Number
foreign import js_alertNumber :: forall eff. Number -> Eff (canvas :: C.Canvas | eff) Unit
foreign import js_requestAnimationFrame :: forall eff. (Eff (canvas :: C.Canvas | eff) Unit) -> Eff (canvas :: C.Canvas | eff) Unit

foreign import js_getNumBarriers :: forall eff. Eff (canvas :: C.Canvas | eff) Int
foreign import js_getNumParticles :: forall eff. Eff (canvas :: C.Canvas | eff) Int
foreign import js_getNumFrames :: forall eff. Eff (canvas :: C.Canvas | eff) Int
