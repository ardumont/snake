module Lib where

-- | Snake's possible direction
data Direction = DirUp | DirRight | DirDown | DirLeft
                 deriving (Eq, Show)

-- | Entities have coordinates
data Coord = Coord Float Float deriving (Eq, Show)

-- | Apple
data Apple = Apple Coord
             deriving (Eq, Show)

-- | Snake eats apple
data Snake = Snake Direction Coord
             deriving (Eq, Show)

-- | World has limits
data Limit = Limit Float Float

-- | From a direction and a set of coordinates, compute the next possible
-- coordinates
nextMove :: Direction -> Coord -> Coord
nextMove DirUp (Coord x y) = Coord x (1 + y)
nextMove DirRight (Coord x y) = Coord (1 + x) y
nextMove DirDown (Coord x y) = Coord x (y - 1)
nextMove DirLeft (Coord x y) = Coord (x - 1) y

-- | Given a set of coordinates and a limit, determine the next possible
-- coordinates.
circularSnake :: Coord -> Limit -> Coord
circularSnake (Coord x y) (Limit w h) =
  case (x < 0, x > w, y < 0, y > h) of
     (True, _, _, _ ) -> Coord w y
     (_, True, _, _)  -> Coord 0 y
     (_, _, True, _)  -> Coord x h
     (_, _, _, True)  -> Coord x 0
     _                -> Coord x y

-- | Given two sets of coordinates, determine if there is a collision or not
collision :: Coord -> Coord -> Bool
collision = (==)

-- | From random int to direction
toDirection :: (Eq a, Num a) => a -> Direction
toDirection 1 = DirUp
toDirection 2 = DirRight
toDirection 3 = DirDown
toDirection 4 = DirLeft
toDirection _ = error "Only 4 possible values are 1, 2, 3, 4."
