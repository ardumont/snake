{-# LANGUAGE TemplateHaskell #-}
module Lib where

import           Control.Lens

-- | Snake's possible dir'
data Direction = DirUp | DirRight | DirDown | DirLeft
                 deriving (Eq, Show)

-- | Entities have coordinates
data Coord = Coord {_x :: Float,
                    _y :: Float} deriving (Eq, Show)

-- | Apple
data Apple = Apple { _ca :: Coord,  -- coordinate apple
                     _ra :: Float } -- radius apple
             deriving (Eq, Show)

-- | Snake eats apple'
data Snake = Snake { _dir :: Direction, -- direction snake
                     _cs  :: Coord,     -- coordinate snake
                     _rs  :: Float }    -- radius snake
             deriving (Eq, Show)

-- | World has limits
type Limit = (Float, Float)

-- | World is made up of one snake, one apple' and limits
data World = World {_snake :: Snake,
                    _apple :: Apple,
                    _limit :: Limit}

makeLenses ''Coord
makeLenses ''Apple
makeLenses ''Snake
makeLenses ''World

-- | From a dir' and a set of coordinates, compute the next possible
-- coordinates
nextMove :: Direction -> Coord -> Coord
nextMove DirUp    (Coord x y) = Coord x (1 + y)
nextMove DirRight (Coord x y) = Coord (1 + x) y
nextMove DirDown  (Coord x y) = Coord x (y - 1)
nextMove DirLeft  (Coord x y) = Coord (x - 1) y

-- | Given a set of coordinates and a limit', determine the next possible
-- coordinates.
circularSnake :: Coord -> Limit -> Coord
circularSnake (Coord x y) (w, h) =
  case (x < 0, x > w, y < 0, y > h) of
       (True, _, _, _ ) -> Coord w y
       (_, True, _, _)  -> Coord 0 y
       (_, _, True, _)  -> Coord x h
       (_, _, _, True)  -> Coord x 0
       _                -> Coord x y

-- | square function (find existing one)
sq x = x * x

-- | Given two sets of coordinates, determine if there is a collide or not
collide :: Snake -> Apple -> Bool
collide (Snake _ (Coord xs ys) rs) (Apple (Coord xa ya) ra) =
  sq (xa - xs) + sq (ya - ys) <= sq rs + sq ra

-- | From random int to dir'
toDirection :: (Eq a, Num a) => a -> Direction
toDirection 1 = DirUp
toDirection 2 = DirRight
toDirection 3 = DirDown
toDirection 4 = DirLeft
toDirection _ = error "Only 4 possible values are 1, 2, 3, 4."

-- | Simple translation +10 on both x and y
translatePos :: Coord -> Coord
translatePos (Coord x y) = Coord (x + 10) (y + 10)

-- | Compute the apple's next position
nextApplePositionPolicy :: Apple -> Apple
nextApplePositionPolicy =  ca `over` translatePos

-- | Snake eats apple'
snakeEatsApple :: t -> World -> World
snakeEatsApple _ world =
  if (world ^. snake) `collide` (world ^. apple)
     then apple `over` nextApplePositionPolicy $ world
     else world

-- | Snake moves in the world
nextMoveWorld :: t -> World -> World
nextMoveWorld _ (World (Snake dir' coord' r') apple' limit') =
  World updatedSnake apple' limit'
  where updatedSnake = Snake dir' (nextMove dir' coord') r'

-- | Snake runs forever and cycle around the world
circularSnakeWorld :: t -> World -> World
circularSnakeWorld _ (World (Snake dir' coord r) apple' limit') =
  World updatedSnake apple' limit'
  where updatedSnake = Snake dir' (circularSnake coord limit') r
