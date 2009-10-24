module Main where

import Control.Monad.State.Lazy
import Path.Evolver.Evolution
import Random

data Turtle = Turtle
              deriving (Show)

instance Individual Turtle where
    individual = return Turtle
    mutate _ = return Turtle

fitness :: Turtle -> Fitness Turtle Int
fitness t = (t, 0)

main :: IO ()
main =
    print $ evalState (evolve fitness) 
          $ mkStdGen 0
