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

-- | Snake eats apple
snakeEatsApple :: t -> World -> World
snakeEatsApple _ world = undefined

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
main = play (InWindow "Snake" (w, h) (0, 0)) -- Window display
         white                               -- Color
         100                                 -- frame per second
         world                               --
         drawWorld                           -- draw the world function
         handleUserInput                     -- Handle user input
         [ nextMoveWorld,                    -- snake moves
           circularSnakeWorld,               -- can move out of bounds
           snakeEatsApple                    -- snake eats apple
         ]
       where snake = Snake DirRight (Coord 0 0)
             apple = Apple (Coord 100 100)
             world = World snake apple (Limit width height)
             w = round width
             h = round height

-- To draw a frame, we position the character sprite at the location as determined by the current state of the world.
-- We shrink the sprite by 50%.

-- | Draw the pix at coordinates
drawCoordinates :: Coord -> Picture -> Picture
drawCoordinates (Coord x y) pix = translate x y (scale 1 1 pix)

-- | Draw the world
drawWorld :: World -> Picture
drawWorld (World (Snake _ cx) (Apple ca) _) =
  pictures [ drawCoordinates cx snakePix, drawCoordinates ca applePix ]

-- | Handle keybindings to change the snake's direction according to keys
handleUserInput :: Event -> World -> World
handleUserInput (EventKey (SpecialKey KeyLeft) Down _ _)  (World (Snake _ c) a l) = World (Snake DirLeft c) a l
handleUserInput (EventKey (SpecialKey KeyRight) Down _ _) (World (Snake _ c) a l) = World (Snake DirRight c) a l
handleUserInput (EventKey (SpecialKey KeyUp) Down _ _)    (World (Snake _ c) a l) = World (Snake DirUp c) a l
handleUserInput (EventKey (SpecialKey KeyDown) Down _ _)  (World (Snake _ c) a l) = World (Snake DirDown c) a l
handleUserInput _ world = world        -- don't change the world in other events
