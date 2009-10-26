module Main where

import Control.Monad.State.Lazy
import Graphics.UI.GLUT as GLUT
import List
import Path.Evolver.Evolution
import Path.Evolver.Turtle  
import Random

fittest :: Ord f => Population Turtle f -> Fitness Turtle f
fittest = maximumBy (\ (_, a) (_, b) -> compare a b)

display :: Show f => Int -> Fitness Turtle f -> IO ()
display generation (Turtle instructions, score) = do
  	clear [ ColorBuffer, DepthBuffer ]
  	loadIdentity
  	renderPrimitive LineStrip
  	    $ mapM_ (\(x, y) -> vertex $ Vertex2 x y)
  	    $ path
  	    $ instructions
  	currentRasterPosition $= Vertex4 (-49) (-49) 0 1
  	renderString Helvetica10 $ "Generation " ++ (show generation) ++ " " ++ (show score)
  	flush

oneGeneration :: (Ord f, Show f, RandomGen g) => (Population Turtle f -> State g (Population Turtle f)) -> Int -> Population Turtle f -> g -> IO ()
oneGeneration mutateFunc generation population g = do
    let (population', g') = runState (mutateFunc population) g
    displayCallback $= (display generation $ fittest population')
    idleCallback $= Just (oneGeneration mutateFunc (generation + 1) population' g')
    postRedisplay Nothing

main :: IO ()
main = do
    initialWindowSize $= Size 400 400
    getArgsAndInitialize
    initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
    createWindow "Path Evolver"
    clearColor $= Color4 0 0 0 0
    matrixMode $= Projection
    loadIdentity
    ortho2D (-50) 50 (-50) 50
    matrixMode $= Modelview 0
    let (population, g) = runState (randomPopulation fitness 100) $ mkStdGen 0
    displayCallback $= (display 1 $ fittest population)
    idleCallback $= Just (oneGeneration (mutatePopulation fitness) 1 population g)
    mainLoop
