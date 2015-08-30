module Snake.Move where

import           Snake.Type

-- | From a direction and a set of coordinates, compute the next possible
-- coordinates
nextMove :: Direction -> Coord -> Coord
nextMove DirUp (Coord x y) = Coord x (1+ y)
nextMove DirRight (Coord x y) = Coord (1+ x) y
nextMove DirDown (Coord x y) = Coord x (1- y)
nextMove DirLeft (Coord x y) = Coord (1- x) y

-- | Given a set of coordinates and a limit, determine the next possible
-- coordinates.
circularSnake :: Coord -> Limit -> Coord
circularSnake (Coord x y) (Limit w h) =
  case (x < 0, x > w, y < 0, y > h) of
     (True, _, _, _ ) -> (w, y)
     (_, True, _, _)  -> (0, y)
     (_, _, True, _)  -> (x, h)
     (_, _, _, True)  -> (x, 0)
     _                -> (x, y)

-- | Given two sets of coordinates, determine if there is a collision or not
collision :: Coord -> Coord -> Bool
collision (Coord xa ya) (Coord xs ys) = xa == xs && ya == ys
n
