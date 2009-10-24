module Main where

import Control.Monad.State.Lazy
import Path.Evolver.BoundedEnum()
import Path.Evolver.Evolution
import Random

data Instruction = Forward
                 | Left
                 | Right
                 deriving (Bounded, Enum, Show)

data Mutation = None
              | Add
              | Drop
              deriving (Bounded, Enum)

data Turtle = Turtle [ Instruction ]
              deriving (Show)

instance Individual Turtle where
    individual = (sequence $ replicate 10 $ State random) >>= return . Turtle
    mutate (Turtle instructions) =
        mutateInstructions instructions >>= return . Turtle
        where mutateInstructions :: RandomGen g => [ Instruction ] -> State g [ Instruction ]
              mutateInstructions (x : xs) = do
                  mutation <- State random
                  x' <- applyMutation x mutation
                  xs' <- mutateInstructions xs
                  return $ x' ++ xs'

              mutateInstructions [ ] = do
                  mutation <- State random
                  case mutation of
                      Add -> State random >>= (\i -> return [ i ])
                      Drop -> return [ ]
                      None -> return [ ]

              applyMutation :: RandomGen g => Instruction -> Mutation -> State g [ Instruction ]
              applyMutation i None = return [ i ]
              applyMutation i Add = State random >>= (\i' -> return [ i, i' ])
              applyMutation _ Drop = return [ ]

fitness :: Turtle -> Fitness Turtle Int
fitness t = (t, 0)

main :: IO ()
main =
    print $ evalState (evolve fitness) 
          $ mkStdGen 0
