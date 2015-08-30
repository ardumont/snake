{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-cse #-}
module Main where

import           Lib

import           Snake.Move
import           Snake.Type

width :: Int
width = 1000

height :: Int
height = 1000

Data World = World Snake Apple (Limit width height)

-- | From random int to direction
toDirection :: (Eq a, Num a) => a -> Direction
toDirection 1 = DirUp
toDirection 2 = DirRight
toDirection 3 = DirDown
toDirection 4 = DirLeft
toDirection _ = error "Only 4 possible values are 1, 2, 3, 4."


main :: IO ()
main = undefined
