module Utils (
      Point2D ( Point2D )
    , Position
    ) where

type Position = (Int, Int)
data Point2D a = Point2D a a deriving (Eq, Show)

instance Num a => Num (Point2D a) where
    Point2D x1 y1 + Point2D x2 y2 = Point2D (x1 + x2) (y1 + y2)
    Point2D x1 y1 - Point2D x2 y2 = Point2D (x1 - x2) (y1 - y2)
    Point2D x1 y1 * Point2D x2 y2 = Point2D (x1 * x2) (y1 * y2)
    abs (Point2D x y) = Point2D (abs x) (abs y)
    signum (Point2D x y) = Point2D (signum x) (signum y)
