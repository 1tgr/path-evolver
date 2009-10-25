module Main where

import Control.Monad.State.Lazy
import Graphics.UI.GLUT
import List
import Path.Evolver.Evolution
import Path.Evolver.Turtle  
import Random

displayPath :: [ Location ] -> IO ()
displayPath ((x1, y1) : (p @ ((x2, y2) : _))) = do
    vertex $ Vertex2 x1 y1
    vertex $ Vertex2 x2 y2
    displayPath p

displayPath _ = return ()

display :: Turtle -> IO ()
display (Turtle instructions) = do
  	clear [ ColorBuffer, DepthBuffer ]
  	loadIdentity
  	renderPrimitive Lines $ displayPath $ path instructions
  	flush

oneGeneration :: (Ord f, RandomGen g) => (Population Turtle f -> State g (Population Turtle f)) -> Population Turtle f -> g -> IO (Population Turtle f, g)
oneGeneration mutateFunc population g = do
    let ret @ (population', _) = runState (mutateFunc population) g
    let fittest = fst $ maximumBy (\ (_, a) (_, b) -> compare a b) $ population'
    displayCallback $= display fittest
    postRedisplay Nothing
    return ret

main :: IO ()
main = do
    getArgsAndInitialize
    initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
    createWindow "Path Evolver"
    clearColor $= Color4 0 0 0 0
    matrixMode $= Projection
    loadIdentity
    ortho (-1) 1 (-1) 1 (-1) 1
    matrixMode $= Modelview 0
    foldM (\(p, g) f -> f p g) (runState (randomPopulation fitness 10000) $ mkStdGen 0)
        $ map oneGeneration
        $ zipWith (flip ($)) [ 1 .. 1000 ] 
        $ repeat
        $ mutatePopulation fitness
    mainLoop
