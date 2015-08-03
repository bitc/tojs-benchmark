{-# LANGUAGE CPP #-}

module Main where

import Base ()

import qualified JavaScript.Web.Canvas as C
import GHCJS.Foreign.Callback

import Barrier
import Particle
import World

main :: IO ()
main = do
    canvas <- C.create 640 480
    js_addCanvasToBody canvas

    ctx <- C.getContext canvas

    numBarriers <- js_getNumBarriers
    numParticles <- js_getNumParticles

    let world = newWorld numBarriers numParticles

    startTime <- js_now

    numFrames <- js_getNumFrames
    animate startTime numFrames ctx world

animate :: Double -> Int -> C.Context -> World -> IO ()
animate startTime framesLeft ctx world = do
    renderWorld ctx world
    let next = stepWorld world
    if framesLeft == 0
        then do
            endTime <- js_now
            js_alertDouble (endTime - startTime)
        else do
            cb <- createCallback (animate startTime (framesLeft - 1) ctx next)
            js_requestAnimationFrame cb

renderWorld :: C.Context -> World -> IO ()
renderWorld ctx world = do
    C.clearRect 0 0 640 480 ctx
    mapM_ renderBarrier (barriers world)
    mapM_ renderParticle (particles world)
    where
    renderParticle :: Particle -> IO ()
    renderParticle (Particle r c x y _ _) = do
        C.beginPath ctx
        C.arc ((fromIntegral x) / 1000) ((fromIntegral y) / 1000) ((fromIntegral r) / 1000) 0 (2 * pi) False ctx
        C.fillStyle 0 c 0 1.0 ctx
        C.fill ctx
    renderBarrier :: Barrier -> IO ()
    renderBarrier (Barrier r x y) = do
        C.beginPath ctx
        C.arc ((fromIntegral x) / 1000) ((fromIntegral y) / 1000) ((fromIntegral r) / 1000) 0 (2 * pi) False ctx
        C.fillStyle 128 128 128 1.0 ctx
        C.fill ctx

createCallback :: IO () -> IO (Callback (IO ()))
createCallback action = do
    cb <- syncCallback ThrowWouldBlock action
    -- In GHCJS all callbacks must be released when they are not being used any
    -- more, or there will be a memory leak.
    --
    -- We are not using the multithreaded scheduler of GHCJS so we can release
    -- the callback immediately from the runtime
    releaseCallback cb
    return cb


#ifdef ghcjs_HOST_OS

foreign import javascript unsafe
    "alert('' + $1)"
    js_alertDouble :: Double -> IO ()

foreign import javascript unsafe
    "new Date().getTime()"
    js_now :: IO Double

foreign import javascript unsafe
    "document.body.appendChild($1)"
    js_addCanvasToBody :: C.Canvas -> IO ()

foreign import javascript unsafe
    "window.requestAnimationFrame($1)"
    js_requestAnimationFrame :: (Callback (IO ())) -> IO ()

foreign import javascript unsafe
    "Number(global_num_barriers)"
    js_getNumBarriers :: IO Int

foreign import javascript unsafe
    "Number(global_num_particles)"
    js_getNumParticles :: IO Int

foreign import javascript unsafe
    "Number(global_num_frames)"
    js_getNumFrames :: IO Int

#else

js_alertDouble :: Double -> IO ()
js_alertDouble = error "js_alert: only available in JavaScript"

js_now :: IO Double
js_now = error "js_now: only available in JavaScript"

js_addCanvasToBody :: C.Canvas -> IO ()
js_addCanvasToBody = error "js_addCanvasToBody: only available in JavaScript"

js_requestAnimationFrame :: (Callback (IO ())) -> IO ()
js_requestAnimationFrame = error "js_requestAnimationFrame: only available in JavaScript"

js_getNumBarriers :: IO Int
js_getNumBarriers = error "js_getNumBarriers: only available in JavaScript"

js_getNumParticles :: IO Int
js_getNumParticles = error "js_getNumParticles: only available in JavaScript"

js_getNumFrames :: IO Int
js_getNumFrames = error "js_getNumFrames: only available in JavaScript"

#endif
