module World where

import Control.Monad.State
import Data.Array
import Data.Tuple
import Prelude

import Barrier
import Environment
import Particle
import PseudoRandom

type World =
    { env :: Environment
    , particles :: Array Particle
    , barriers :: Array Barrier
    }

newWorld :: Int -> Int -> World
newWorld numBarriers numParticles =
    let w = 640000
        h = 480000
        g = 80

        rg = newRandomState
        result = (flip runState) rg $ do
            bs <- replicateM numBarriers (randomBarrier w h)
            ps <- replicateM numParticles randomParticle
            return $ Tuple bs ps
    in case result of
        Tuple (Tuple bs' ps') _ ->
            { env:
                { width: w
                , height: h
                , gravity: g
                , dampingA: Tuple 199 100
                , dampingB: Tuple 99 100
                }
            , particles: ps'
            , barriers: bs'
            }

randomParticle :: State RandomState Particle
randomParticle = do
    r <- getRandomRange 2000 5000
    c <- getRandomRange 128 255
    x <- getRandomRange 0 640000
    y <- getRandomRange 0 100000
    vx <- getRandomRange (-2000) 2000
    vy <- getRandomRange (-2000) 0
    return (newParticle r c x y vx vy)

randomBarrier :: Int -> Int -> State RandomState Barrier
randomBarrier w h = do
    r <- getRandomRange 10000 50000
    x <- getRandomRange 0 w
    y <- getRandomRange 100000 h
    return
        { barrierRadius: r
        , barrierX: x
        , barrierY: y
        }


stepWorld :: World -> World
stepWorld world =
    let particles' = map (stepParticle (world.env) (world.barriers)) (world.particles)
    in world { particles = particles' }
