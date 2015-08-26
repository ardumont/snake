{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
module Main where

import           ActionKid
import           ActionKid.Utils
import           Control.Lens
import           Control.Monad.State
import           Data.List           (elemIndex)
import           Data.Monoid         ((<>))
import           System.Random

data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Show, Eq)

-- This is an example of how to use ActionKid to make games with Haskell.
-- ActionKid is inspired by Actionscript, so you will see some
-- similarities.
--
-- Every video game has some objects on the screen that interact with each
-- other. For example, in a game of Mario, you will have Mario himself,
-- goombas, mushrooms, pipes etc. These objects are called "movie clip"s in
-- ActionKid terminology. Any data type that is an instance of the
-- MovieClip class can be used in your game.

-- So first, make a data type for every object you will have in your game.
-- In this demo game, we just have a snake that will move around.
--
-- Every constructor must have `Attributes` as it's last field.
data Snake = Snake { _direction :: Direction, _sa :: Attributes }
data Apple = Apple { _aa :: Attributes }

-- Ok, you have a Snake type. Now before you can use it in your game,
-- make it an instance of MovieClip. You can do this automatically with
-- `deriveMC`:
deriveMC ''Snake
deriveMC ''Apple

-- Now that the snake is a MovieClip, you can write code like this:
--
-- > snake.x += 10
--
-- and the snake will move 10 pixels to the right!
-- More on this later.

-- You also need a data type that will be the game state.
data GameState = GameState {
                    _snake :: Snake,
                    _apple :: Apple,
                    _ga    :: Attributes
                 }

-- Use this convenience function to make MovieClip instances
-- for your data types automatically.
deriveMC ''GameState

-- Next, I suggest you make lenses for all of your data types.
-- If you don't know how lenses work, check out the intro README here:
-- https://github.com/ekmett/lens
--
-- and this tutorial:
-- https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/basic-lensing
--
-- Lenses are great for working with nested data structures, and writing
-- functional code that looks imperative. Both are big plusses for game
-- development.
-- So this step is optional, but recommended.
makeLenses ''Snake
makeLenses ''Apple
makeLenses ''GameState

-- Finally, you need to make both Snake and GameState
-- instances of Renderable. This defines how they will be shown on the
-- screen.
--
-- The `render` function returns a Gloss Picture:
--
-- http://hackage.haskell.org/package/gloss-1.8.2.2/docs/Graphics-Gloss-Data-Picture.html

instance Renderable Snake where
    render p = color green $ box 50 50

instance Renderable Apple where
    render p = color red $ box 20 20

-- Here we are just rendering the snake as a grey box. With ActionKid,
-- you can also use an image from your computer instead:
--
-- > render p = image "images/snake.png"

-- To render the game state, we just render the snake.
-- To do that, use the `display` function. `display` will render
-- the snake at the right x and y coordinates.
instance Renderable GameState where
    render gs = display (_apple gs) <> display (_snake gs)

-- If the game state has multiple items, you can render them all by
-- concatenating them:
--
-- > render gs = display (_player1 gs) <> display (_player2 gs)

-- this is the default game state. The snake starts at coordinates (0,0)
-- (the bottom left of the screen).
-- NOTE: For the `Attributes` field, you can just use `def`. This will set
-- the correct default attributes for an object.
--
-- So this creates the game state with a snake in it. Both have default
-- attributes.

initGameState :: Snake -> Apple -> GameState
initGameState s a = GameState s a def

-- defaultGameState :: GameState
-- defaultGameState = initGameState (Snake DirDown def) (Apple $ apple_def 100 100)

-- All of the core game logic takes place in this monad transformer stack.
-- The State is the default game state we just made.
type GameMonad a = StateT GameState IO a

--------------------------------------------------------------------------
--------------------------------------------------------------------------
-- Ok, now we are done specifying all the data types and how they should
-- look! Now it's time to implement the core game logic. There are two
-- functions you need to define:
--
-- 1. An event handler (for key presses/mouse clicks)
-- 2. A game loop.
--
-- The event handler listens for user input, and moves the snake etc.
-- The game loop is where the rest of the logic happens: firing bullets,
-- hitting an enemy, ani mations etc etc.
--------------------------------------------------------------------------
--------------------------------------------------------------------------

-- This is the event handler. Since we are using lenses, this logic is
-- really easy to write.
eventHandler :: Event -> GameMonad ()
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) = snake.direction .= DirLeft
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) = snake.direction .= DirRight
eventHandler (EventKey (SpecialKey KeyUp) Down _ _) = snake.direction .= DirUp
eventHandler (EventKey (SpecialKey KeyDown) Down _ _) = snake.direction .= DirDown
eventHandler _ = return ()

world_dim :: Float
world_dim = 1000

snake_speed :: Num a => a
snake_speed = 10

runForEver :: Float -> GameMonad ()
runForEver _ = do
  gs <- get
  case gs ^. snake . direction of
    DirUp -> snake.y += snake_speed
    DirRight -> snake.x += snake_speed
    DirDown -> snake.y -= snake_speed
    DirLeft -> snake.x -= snake_speed
  return ()

myRandom :: Int -> Float -> Float -> IO [Float]
myRandom n min max = do
  g <- getStdGen
  return $ take n (randomRs (min, max) g :: [Float])

toDirection :: (Eq a, Num a) => a -> Direction
toDirection 1 = DirUp
toDirection 2 = DirRight
toDirection 3 = DirDown
toDirection 4 = DirLeft
toDirection _ = undefined

snake_def :: Float -> Float -> Attributes
snake_def x y = Attributes x y 1.0 1.0 True 1

apple_def :: Float -> Float -> Attributes
apple_def x y = Attributes x y 1.0 1.0 True 2

-- Now lets run the game! The run function takes:
-- 1. the title for the window of the game
-- 2. the size of the window
-- 3. the initial game state
-- 4. the eventHandler function
-- 5. the main loop function
main :: IO ()
main = do
  rndPos <- myRandom 4 1 world_dim
  rndDirection <- myRandom 1 1 4
  let snake = (Snake ((toDirection . round . (!! 0)) rndDirection) (snake_def (rndPos!!0) (rndPos!!1)))
  let apple = Apple $ apple_def (rndPos!!2) (rndPos!!3)
  run "Snake" (round(world_dim), round(world_dim)) (initGameState snake apple) eventHandler runForEver
