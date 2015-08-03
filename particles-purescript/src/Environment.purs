module Environment where

import Data.Tuple

type Environment =
    { width :: Int
    , height :: Int
    , gravity :: Int
    , dampingA :: Tuple Int Int
    , dampingB :: Tuple Int Int
    }
