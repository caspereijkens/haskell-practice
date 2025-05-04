module Main (main) where

import GameOfLife.Core
import GameOfLife.Render
import GameOfLife.Types
import GameOfLife.Patterns

import System.Console.ANSI (clearScreen)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  let config = Config 20 20 False
  initialGrid <- insertRandomPatterns config (mkGrid config) 30
  runSimulation config initialGrid

runSimulation :: Config -> Grid -> IO ()
runSimulation cfg grid = do
  clearScreen
  putStrLn $ render grid
  threadDelay 100000  -- 0.5 second delay
  runSimulation cfg (nextGen cfg grid)
