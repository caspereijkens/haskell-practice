module Main (main) where

-- Temporary minimal main for compilation
main :: IO ()
main = putStrLn "Game of Life (entry point disabled during development)"

{-
-- Full version to restore later:
import GameOfLife.Core
import GameOfLife.Render
import GameOfLife.Types
import GameOfLife.Patterns  -- When ready

main :: IO ()
main = do
  let config = Config 20 20 False
      initialGrid = insertPattern glider (mkGrid config)
  runSimulation config initialGrid

runSimulation :: Config -> Grid -> IO ()
runSimulation cfg grid = do
  putStrLn $ render grid
  putStrLn "Press Enter to continue..."
  _ <- getLine
  runSimulation cfg (nextGen cfg grid)
-}
