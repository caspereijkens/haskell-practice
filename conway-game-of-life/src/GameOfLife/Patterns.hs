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
    ((_, _), (patternH, patternW)) = bounds pattern

    updates = [ (pos, pattern ! (y', x'))
              | y <- [py .. py + patternH]
              , x <- [px .. px + patternW]
              , let y' = (y - py) `mod` (patternH + 1)
              , let x' = (x - px) `mod` (patternW + 1)
              , let pos = (y `mod` height cfg, x `mod` width cfg)
              , inBounds pos ]

    inBounds (y, x) = y >= 0 && y <= (height cfg -1) && x >= 0 && x <= (width cfg -1)

glider :: Grid
glider = listArray ((0, 0), (2, 2))
  [ False, True, False
  , False, False, True
  , True,  True,  True ]
