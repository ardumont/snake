module Main where

import           Graphics.Gloss.Game
import           Lib

width :: Float
width = 1000

height :: Float
height = 1000

applePix :: Picture
applePix = bmp "resources/apple.bmp"

snakePix :: Picture
snakePix = bmp "resources/snake.bmp"

data World = World Snake Apple Limit

-- | Snake moves in the world
nextMoveWorld :: t -> World -> World
nextMoveWorld _ (World (Snake dir coord) apple limit) =
  World updatedSnake apple limit
  where updatedSnake = Snake dir (nextMove dir coord)

-- | Snake runs forever and cycle around the world
circularSnakeWorld :: t -> World -> World
circularSnakeWorld _ (World (Snake dir coord) apple limit) =
  World updatedSnake apple limit
  where updatedSnake = Snake dir (circularSnake coord limit)

-- data example
-- apple_1 :: Apple
-- apple_1 = Apple { _coorda = Coord 10 10 }
-- snake_1 = Snake { _direction = DirUp,
--                   _coords = Coord 10 10 }

-- This starts our game in a window with a give size, running at 30 frames per second.
-- The argument 'World (0, 0)' is the initial state of our game world, where our character is at the centre of the
-- window.

-- play
--   :: Display
--      -> Color
--      -> Int
--      -> world
--      -> (world -> Picture)
--      -> (Event -> world -> world)
--      -> [Float -> world -> world]
--      -> IO ()

main :: IO ()
main = play (InWindow "Snake" (w, h) (50, 50)) white 30 world drawWorld handle [ nextMoveWorld, circularSnakeWorld ]
       where snake = Snake DirUp (Coord 0 0)
             apple = Apple (Coord 10 10)
             world = World snake apple (Limit width height)
             w = round width
             h = round height

-- To draw a frame, we position the character sprite at the location as determined by the current state of the world.
-- We shrink the sprite by 50%.

drawCoordinates :: Coord -> Picture -> Picture
drawCoordinates (Coord x y) pix = translate x y (scale 0.5 0.5 pix)

drawWorld :: World -> Picture
drawWorld (World (Snake _ cx) (Apple ca) _) =
  pictures [ drawCoordinates cx snakePix, drawCoordinates ca applePix ]

-- Whenever any of the keys 'a', 'd', 'w', or 's' have been pushed down, move our character in the corresponding
-- direction.
handle :: t -> t1 -> t1
-- handle (EventKey (Char 'a') Down _ _) (World (x, y)) = World (x - 10, y)
-- handle (EventKey (Char 'd') Down _ _) (World (x, y)) = World (x + 10, y)
-- handle (EventKey (Char 'w') Down _ _) (World (x, y)) = World (x, y + 10)
-- handle (EventKey (Char 's') Down _ _) (World (x, y)) = World (x, y - 10)
handle _ world = world        -- don't change the world in case of any other events
