module Path.Evolver.Turtle where
  
import Control.Monad.State.Lazy
import Path.Evolver.BoundedEnum()
import Path.Evolver.Evolution
import Random

data Instruction = Forward
                 | TurnLeft
                 | TurnRight
                 deriving (Bounded, Enum, Show)

data Mutation = AddInstruction
              | DropInstruction
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
                      AddInstruction -> State random >>= (\i -> return [ i ])
                      DropInstruction -> return [ ]

              applyMutation :: RandomGen g => Instruction -> Mutation -> State g [ Instruction ]
              applyMutation i AddInstruction = State random >>= (\i' -> return [ i, i' ])
              applyMutation _ DropInstruction = return [ ]

type Location = (Double, Double)

data TurtleState = TurtleState
                 {
                     location :: Location,
                     direction :: Double
                 }

path :: [ Instruction ] -> [ Location ]
path = (++ [ (0, 0) ])
     . ((0, 0) : )
     . (flip evalState) (TurtleState { location = (0, 0), direction = 0 })
     . mapM applyInstructionM
    where applyInstruction :: Instruction -> TurtleState -> TurtleState
          applyInstruction Forward state @ TurtleState { location = (x, y), direction = d } = state { location = (x + sin d, y + cos d) }
          applyInstruction TurnLeft state @ TurtleState { direction = d } = state { direction = d - dd }
          applyInstruction TurnRight state @ TurtleState { direction = d } = state { direction = d + dd }
          dd = (2 * pi) / 10

          applyInstructionM :: Instruction -> State TurtleState Location
          applyInstructionM instruction = do
              state <- get
              let state' @ TurtleState { location = l } = applyInstruction instruction state
              put state'
              return l

centre :: [ Location ] -> Location
centre p = (sumX / pathLength, sumY / pathLength)
    where (sumX, sumY) = foldl1 (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) p
          pathLength = fromRational $ toRational $ length p

stats :: (Floating a, Ord a) => [ a ] -> (a, a, a, a, a, a)
stats (x:xs) = finish . foldl stats' (x, x, x, x*x, 1) $ xs
  where stats' (mx, mn, s, ss, n) xx = (max xx mx, min xx mn, s + xx, ss + xx * xx, n+1)
        finish (mx, mn, s, ss, n) = (mx, mn, av, va, stdev, n)
          where av = s/n
                va = ss/(n-1) - n*av*av/(n-1)
                stdev = sqrt va

stats [ ] = error "stats: empty list"

fitness :: Turtle -> Fitness Turtle (Double, Double, Double)
fitness turtle @ (Turtle instructions) =
    let p = path instructions
        square x = x * x
        (centreX, centreY) = centre p
        (_, _, av, _, stdev, _) = stats
                                $ map (\(x, y) -> square (x - centreX) + square (y - centreY))
                                $ p
        score = av / (1 + stdev)
    in (turtle, (score, av, stdev))
