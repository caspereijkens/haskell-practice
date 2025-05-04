module GameOfLife.Patterns
  ( insertPatternAt, glider, blinker, block, insertRandomPattern, insertRandomPatterns) where

import GameOfLife.Types
import Data.Array (listArray, (!), bounds, (//))  
import Control.Monad (foldM)
import System.Random (randomRIO)

insertPatternAt :: Config -> Position -> Grid -> Grid -> Grid
insertPatternAt cfg (px, py) pattern target = 
    target // updates
  where
    ((_, _), (patternH, patternW)) = bounds pattern

    updates = [ (pos, pattern ! (y', x'))
              | y <- [py .. py + patternH]
              , x <- [px .. px + patternW]
              , let y' = (y - py) `mod` (patternH + 1)
              , let x' = (x - px) `mod` (patternW + 1)
              , let pos = (y `mod` height cfg, x `mod` width cfg)
              , inBounds pos ]

    inBounds (y, x) = y >= 0 && y <= (height cfg -1) && x >= 0 && x <= (width cfg -1)

insertRandomPattern :: Config -> Grid -> IO Grid
insertRandomPattern cfg grid = do
  let patterns = [glider, blinker, block]
  patternIdx <- randomRIO (0, length patterns - 1)
  let chosenPattern = patterns !! patternIdx

  let ((_, _), (pHeight, pWidth)) = bounds chosenPattern

  randY <- randomRIO (0, height cfg - pHeight - 1)
  randX <- randomRIO (0, width cfg - pWidth - 1)

  return $ insertPatternAt cfg (randY, randX) chosenPattern grid

insertRandomPatterns :: Config -> Grid -> Int -> IO Grid
insertRandomPatterns cfg grid numPatterns = foldM insertOne grid [1..numPatterns]
  where
    patterns = [glider, blinker, block]
    insertOne g _ = do
      patternIdx <- randomRIO (0, length patterns - 1)
      let chosenPattern = patterns !! patternIdx
      let ((_, _), (pMaxY, pMaxX)) = bounds chosenPattern

      randY <- randomRIO (0, height cfg - (pMaxY + 1))
      randX <- randomRIO (0, width cfg - (pMaxX + 1))

      return $ insertPatternAt cfg (randY, randX) chosenPattern g

glider :: Grid
glider = listArray ((0, 0), (2, 2))
  [ False, True, False
  , False, False, True
  , True,  True,  True ]

blinker :: Grid
blinker = listArray ((0,0),(0,2))
  [True, True, True]
  
block :: Grid
block = listArray ((0,0),(1,1))
  [True, True,
   True, True]
