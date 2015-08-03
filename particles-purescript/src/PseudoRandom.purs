module PseudoRandom
    ( RandomState()
    , newRandomState
    , nextRandom
    , getRandom
    , getRandomRange
    ) where

import Control.Monad.State
import Control.Monad.State.Class
import Data.Int.Bits
import Data.Tuple
import Prelude

data RandomState = RandomState Int Int Int Int

newRandomState :: RandomState
newRandomState = RandomState 123456789
                             362436069
                             521288629
                             88675123

nextRandom :: RandomState -> Tuple Int RandomState
nextRandom (RandomState x y z w) =
    let t = x .^. (x `shl` 11)
        x' = y
        y' = z
        z' = w
        w' = w .^. (w `shr` 19) .^. (t .^. (t `shr` 8))
    in Tuple w' (RandomState x' y' z' w')

-- Utility Functions

getRandom :: State RandomState Int
getRandom = do
    rg <- get
    case nextRandom rg of
        Tuple val rg' -> do
            put rg'
            return val

getRandomRange :: Int -> Int -> State RandomState Int
getRandomRange low high = do
    r <- getRandom
    let clipped = r `mod` (high - low)
    return (clipped + low)
