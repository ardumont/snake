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

-- | Compute the apple's next position
nextApplePosition :: Apple -> Apple
nextApplePosition (Apple (Coord x y)) = (Apple $ Coord (x+10) (y+10))

-- | Snake eats apple
snakeEatsApple :: t -> World -> World
snakeEatsApple _ world@(World snake apple limit) =
  if (collision snake apple)
  then (World snake (nextApplePosition apple) limit)
  else world

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
       where snake = Snake DirRight (Coord 0 0)  -- FIXME randomly set the snake's direction + initial position
             apple = Apple (Coord 100 100)       -- FIXME random set the apple's position
             world = World snake apple (Limit width height)
             w = round width
             h = round height

-- | Draw the pix at coordinates
drawCoordinates :: Coord -> Picture -> Picture
drawCoordinates (Coord x y) pix = translate x y (scale 0.25 0.25 pix)

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
