module Particle where

import Barrier
import Data.List
import Environment

data Particle = Particle
    { radius :: Int
    , color :: Int
    , posX :: Int
    , posY :: Int
    , oldX :: Int
    , oldY :: Int
    }
    deriving (Show)

newParticle :: Int -> Int -> Int -> Int -> Int -> Int -> Particle
newParticle r c x y vx vy =
    Particle r c x y (x - vx) (y - vy)

stepParticle :: Environment -> [Barrier] -> Particle -> Particle
stepParticle env barriers p =
    let stepped = verlet env p
    in foldl' barrierCollide stepped barriers

verlet :: Environment -> Particle -> Particle
verlet env p@(Particle _ _ x y ox oy) =
    let dAN = fst (dampingA env)
        dAD = snd (dampingA env)
        dBN = fst (dampingB env)
        dBD = snd (dampingB env)
        newx = (dAN * x) `div` dAD  -  (dBN * ox) `div` dBD
        newy = (dAN * y) `div` dAD  -  (dBN * oy) `div` dBD  +  (gravity env)
    in p
        { oldX = x
        , oldY = y
        , posX = newx
        , posY = newy
        }

barrierCollide :: Particle -> Barrier -> Particle
barrierCollide p b =
    let dx, dy :: Double
        dx = fromIntegral (posX p - barrierX b)
        dy = fromIntegral (posY p - barrierY b)
        distanceSquared, minDistance :: Double
        distanceSquared = (dx * dx) + (dy * dy)
        minDistance = fromIntegral (barrierRadius b + radius p)
    in if distanceSquared >= minDistance * minDistance
        then p
        else
            let distance :: Double
                distance = sqrt distanceSquared
                p' = p
                    { posX = barrierX b + truncate ((dx * minDistance) / distance)
                    , posY = barrierY b + truncate ((dy * minDistance) / distance)
                    }
            in  p'
