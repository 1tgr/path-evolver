module Main where

import Control.Monad.State.Lazy
import Graphics.UI.GLUT
import List
import Path.Evolver.Evolution
import Path.Evolver.Turtle  
import Random

fittest :: Ord f => Population Turtle f -> Turtle
fittest = fst . maximumBy (\ (_, a) (_, b) -> compare a b)

display :: Turtle -> IO ()
display (Turtle instructions) = do
  	clear [ ColorBuffer, DepthBuffer ]
  	loadIdentity
  	renderPrimitive LineStrip
  	    $ mapM_ (\(x, y) -> vertex $ Vertex2 x y)
  	    $ path
  	    $ instructions
  	flush

oneGeneration :: (Ord f, RandomGen g) => (Int -> Population Turtle f -> State g (Population Turtle f)) -> Int -> Population Turtle f -> g -> IO ()
oneGeneration mutateFunc generation population g = do
    let (population', g') = runState (mutateFunc generation population) g
    displayCallback $= (display $ fittest population')
    idleCallback $= Just (oneGeneration mutateFunc (generation + 1) population' g')
    postRedisplay Nothing

main :: IO ()
main = do
    getArgsAndInitialize
    initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
    createWindow "Path Evolver"
    clearColor $= Color4 0 0 0 0
    matrixMode $= Projection
    loadIdentity
    ortho2D (-50) 50 (-50) 50
    matrixMode $= Modelview 0
    let (population, g) = runState (randomPopulation fitness 10000) $ mkStdGen 0
    displayCallback $= (display $ fittest population)
    idleCallback $= Just (oneGeneration (mutatePopulation fitness) 1 population g)
    mainLoop
