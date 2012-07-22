module Pos
    where

data Pos = Pos { posX :: !Int, 
                 posY :: !Int}
         deriving (Eq, Ord, Show)

instance Num Pos where
    (Pos x1 y1) + (Pos x2 y2) = Pos (x1 + x2) (y1 + y2)
    (Pos x1 y1) - (Pos x2 y2) = Pos (x1 - x2) (y1 - y2)
    (Pos x1 y1) * (Pos x2 y2) = Pos (x1 * x2) (y1 * y2)
    negate (Pos x y) = Pos (negate x) (negate y)
    abs (Pos x y) = Pos (abs x) (abs y)
    signum (Pos x y) = Pos (signum x) (signum y)
    fromInteger i = Pos (fromInteger i) (fromInteger i)

mapPos :: (Int -> Int) -> Pos -> Pos
mapPos f (Pos x y) = Pos (f x) (f y)

distance :: Pos -> Pos -> Int
distance (Pos x1 y1) (Pos x2 y2) = abs (x1 - x2) + abs (y1 - y2)