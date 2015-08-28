{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
module Main where

import           ActionKid
import           ActionKid.Utils
import           Control.Lens
import           Control.Monad.State
import           Data.List                 (elemIndex)
import           Data.Monoid               ((<>))
import           Graphics.Gloss.Data.Color
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

worldDim :: Float
worldDim = 1000

snakeSpeed :: Num a => a
snakeSpeed = 5

snakeColor :: Color
snakeColor = dark green

appleColor :: Color
appleColor = red

snakeWidth, snakeHeight :: Num a => a
snakeWidth = 1
snakeHeight = 1

appleWidth, appleHeight :: Num a => a
appleWidth = 1
appleHeight = 1

instance Renderable Snake where
    render p = color snakeColor $ box snakeWidth snakeHeight

instance Renderable Apple where
    render p = color appleColor $ box appleWidth appleHeight

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

-- | Initialize the game's state with a snake and an apple
-- the rnd is an infinite array of float used for `random` position
initGameState :: Snake -> Apple -> GameState
initGameState s a = GameState s a def

defaultGameState :: GameState
defaultGameState = initGameState (Snake DirDown def) (Apple $ appleDef 100 100)

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

-- type GameMonad a = StateT GameState IO a
-- get :: MonadState s m => m s
-- (^.) :: s -> Getting a s a -> a
-- (.=) :: MonadState s m => ASetter s s a b -> b -> m ()
-- (+=) :: (Num a, MonadState s m) => ASetter' s a -> a -> m ()
-- (-=) :: (Num a, MonadState s m) => ASetter' s a -> a -> m ()

-- | Move snake according to its direction
moveSnake :: GameState -> GameMonad ()
moveSnake gs = case gs ^. snake . direction of
    DirUp -> snake.y += snakeSpeed
    DirRight -> snake.x += snakeSpeed
    DirDown -> snake.y -= snakeSpeed
    DirLeft -> snake.x -= snakeSpeed

-- | Snake is boundless so gets back the other side every time
boundlessSnake :: GameState -> GameMonad ()
boundlessSnake gs = do
  when (gs ^. snake.x < 0) $ do
     snake.x .= worldDim
  when (gs ^. snake.x > worldDim) $ do
     snake.x .= 0
  when (gs ^. snake.y < 0) $ do
     snake.y .= worldDim
  when (gs ^. snake.y > worldDim) $ do
     snake.y .= 0

snakeAteApple :: GameState -> GameMonad ()
snakeAteApple _ = return ()

runForEver :: Float -> GameMonad ()
runForEver _ = do
  gs <- get
  moveSnake gs
  boundlessSnake gs
  snakeAteApple gs
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

-- | Initialize entity's default attributes
entityAttributesDef :: Int -> Float -> Float -> Attributes
entityAttributesDef index x y = Attributes x y 10.0 10.0 True index

-- | Initialize Snake's attributes
snakeDef :: Float -> Float -> Attributes
snakeDef = entityAttributesDef 1

-- | Initialize Apple's attributes
appleDef :: Float -> Float -> Attributes
appleDef = entityAttributesDef 2

-- | Initialize a snake's start position and direction given 2 arrays of random
-- values.
initSnake :: [Float] -> [Float] -> Snake
initSnake rndDir rndPos = Snake dir (snakeDef x y)
  where x = rndPos!!0
        y = rndPos!!1
        dir = toDirection . round . (!! 0) $ rndDir

-- | Initialize an apple's start position given an array of random position
initApple :: [Float] -> Apple
initApple rndPos = Apple $ appleDef x y
  where x = rndPos!!0
        y = rndPos!!1

-- Now lets run the game! The run function takes:
-- 1. the title for the window of the game
-- 2. the size of the window
-- 3. the initial game state
-- 4. the eventHandler function
-- 5. the main loop function
main :: IO ()
main = do
  rndPosApple <- myRandom 2 1 worldDim
  rndPosSnake <- myRandom 2 1 worldDim
  rndDirection <- myRandom 1 1 4
  let snakeStart = initSnake rndDirection rndPosSnake
      appleStart = initApple rndPosApple
  run "Snake" (dim, dim) (initGameState snakeStart appleStart) eventHandler runForEver
  where dim = round(worldDim)
