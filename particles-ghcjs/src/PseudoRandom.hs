module PseudoRandom
    ( RandomState
    , newRandomState
    , nextRandom
    , getRandom
    , getRandomRange
    ) where

import Control.Monad.State
import Data.Bits

data RandomState = RandomState Int Int Int Int

newRandomState :: RandomState
newRandomState = RandomState 123456789
                             362436069
                             521288629
                             88675123

nextRandom :: RandomState -> (Int, RandomState)
nextRandom (RandomState x y z w) =
    let t = x `xor` (x `shiftL` 11)
        x' = y
        y' = z
        z' = w
        w' = w `xor` (w `shiftR` 19) `xor` (t `xor` (t `shiftR` 8))
    in (w', RandomState x' y' z' w')

-- Utility Functions

getRandom :: State RandomState Int
getRandom = do
    rg <- get
    let (val, rg') = nextRandom rg
    put rg'
    return val

getRandomRange :: (Int, Int) -> State RandomState Int
getRandomRange (low, high) = do
    r <- getRandom
    let clipped = r `mod` (high - low)
    return (clipped + low)

