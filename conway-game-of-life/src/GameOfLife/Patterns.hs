module GameOfLife.Patterns
  ( insertPatternAt
  , glider
  ) where

import GameOfLife.Types
import Data.Array (listArray, (!), bounds, (//))  

insertPatternAt :: Config -> Position -> Grid -> Grid -> Grid
insertPatternAt cfg (px, py) pattern target = 
    target // updates
  where
    ((minY, minX), (maxY, maxX)) = bounds target
    height = maxY - minY + 1
    width  = maxX - minX + 1

    ((_, _), (patternH, patternW)) = bounds pattern

    updates = [ (pos, pattern ! (y', x'))
              | y <- [py .. py + patternH]
              , x <- [px .. px + patternW]
              , let y' = (y - py) `mod` (patternH + 1)
              , let x' = (x - px) `mod` (patternW + 1)
              , let pos = ((y - minY) `mod` height + minY, (x - minX) `mod` width + minX)
              , inBounds pos ]

    inBounds (y, x) = y >= minY && y <= maxY && x >= minX && x <= maxX

glider :: Grid
glider = listArray ((0, 0), (2, 2))
  [ False, True, False
  , False, False, True
  , True,  True,  True ]
