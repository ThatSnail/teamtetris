module Utils (
      Point2D ( Point2D )
    , Position
    , replace2
    , addPos
    ) where

import Data.List.Split

type Position = (Int, Int)
data Point2D a = Point2D a a deriving (Eq, Show)

addPos :: Position -> Position -> Position
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

instance Num a => Num (Point2D a) where
    Point2D x1 y1 + Point2D x2 y2 = Point2D (x1 + x2) (y1 + y2)
    Point2D x1 y1 - Point2D x2 y2 = Point2D (x1 - x2) (y1 - y2)
    Point2D x1 y1 * Point2D x2 y2 = Point2D (x1 * x2) (y1 * y2)
    abs (Point2D x y) = Point2D (abs x) (abs y)
    signum (Point2D x y) = Point2D (signum x) (signum y)

replace :: Int -> a -> [a] -> [a]
replace n x a = c ++ (x : ds)
    where
        (c, _:ds) = splitAt n a

replace2 :: Int -> Int -> a -> [[a]] -> [[a]]
replace2 m n x a = c ++ ((replace n x d) : ds)
    where
        (c, d:ds) = splitAt m a
