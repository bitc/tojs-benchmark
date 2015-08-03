module Particle where

import Data.Foldable
import Data.Int
import Data.Tuple
import Math (sqrt)
import Prelude

import Barrier
import Environment

type Particle =
    { radius :: Int
    , color :: Int
    , posX :: Int
    , posY :: Int
    , oldX :: Int
    , oldY :: Int
    }

newParticle :: Int -> Int -> Int -> Int -> Int -> Int -> Particle
newParticle r c x y vx vy =
    { radius: r
    , color: c
    , posX: x
    , posY: y
    , oldX: x - vx
    , oldY: y - vy
    }

stepParticle :: Environment -> Array Barrier -> Particle -> Particle
stepParticle env barriers p =
    let stepped = verlet env p
    in foldl barrierCollide stepped barriers

verlet :: Environment -> Particle -> Particle
verlet env p =
    let dAN = fst env.dampingA
        dAD = snd env.dampingA
        dBN = fst env.dampingB
        dBD = snd env.dampingB
        newx = (dAN * p.posX) `div` dAD  -  (dBN * p.oldX) `div` dBD
        newy = (dAN * p.posY) `div` dAD  -  (dBN * p.oldY) `div` dBD  +  env.gravity
    in p
        { oldX = p.posX
        , oldY = p.posY
        , posX = newx
        , posY = newy
        }

barrierCollide :: Particle -> Barrier -> Particle
barrierCollide p b =
    let dx :: Number
        dx = toNumber (p.posX - b.barrierX)
        dy :: Number
        dy = toNumber (p.posY - b.barrierY)
        distanceSquared :: Number
        distanceSquared = (dx * dx) + (dy * dy)
        minDistance :: Number
        minDistance = toNumber (b.barrierRadius + p.radius)
    in if distanceSquared >= minDistance * minDistance
        then p
        else
            let distance :: Number
                distance = sqrt distanceSquared
                p' = p
                    { posX = b.barrierX + truncate ((dx * minDistance) / distance)
                    , posY = b.barrierY + truncate ((dy * minDistance) / distance)
                    }
            in  p'

foreign import truncate :: Number -> Int
