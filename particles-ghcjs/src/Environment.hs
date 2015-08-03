module Environment where

data Environment = Environment
    { width :: Int
    , height :: Int
    , gravity :: Int
    , dampingA :: (Int, Int)
    , dampingB :: (Int, Int)
    }
