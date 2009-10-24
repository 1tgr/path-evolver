{-# LANGUAGE MultiParamTypeClasses #-}
module Path.Evolver.Evolution where
  
import Control.Monad.State.Lazy
import Control.Parallel.Strategies
import Debug.Trace
import Random
import List

class Individual i where
    individual :: RandomGen g => State g i
    mutate :: RandomGen g => i -> State g i

type Fitness i f = (i, f)
type Population i f = [ Fitness i f ]

randomPopulation :: (Individual i, RandomGen g) => (i -> Fitness i f) -> Int -> State g (Population i f)
randomPopulation fitness count = sequence 
                               $ replicate count 
                               $ individual >>= return . fitness

mutatePopulation :: (Individual i, Ord f, Show f, RandomGen g) => (i -> Fitness i f) -> Int -> Population i f -> State g (Population i f)
mutatePopulation fitness generation population = do
    individuals' <- sequence $ replicate (count - 1) $ mutate fittestIndividual
    let population' = parMap rwhnf fitness individuals'
    return (fittest : population')
    where fittest @ (fittestIndividual, fittestScore) = maximumBy (\ (_, a) (_, b) -> compare a b) population
          message = "Generation " ++ (show generation) ++ ": " ++ (show $ fittestScore)
          count = trace message $ length population

evolve :: (Individual i, Ord f, Show f, RandomGen g) => (i -> Fitness i f) -> State g (Fitness i f)
evolve fitness = do
    initialPopulation <- randomPopulation fitness 100
    finalPopulation <- foldM (flip ($)) initialPopulation 
                    $ zipWith (flip ($)) [ 1 .. 1000 ] 
                    $ repeat
                    $ mutatePopulation fitness
    return $ maximumBy (\ (_, a) (_, b) -> compare a b) finalPopulation

