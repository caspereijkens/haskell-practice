module TestUtils
    ( toListGrid
    , mkTestGrid
    ) where

import GameOfLife.Types
import Data.Array (listArray, (!), bounds)

-- Convert Grid array to nested lists for testing
toListGrid :: Grid -> [[Bool]]
toListGrid g = 
  let ((minY,minX),(maxY,maxX)) = bounds g
  in [[ g ! (y,x) | x <- [minX..maxX] ] | y <- [minY..maxY] ]

-- Helper to create test grids
mkTestGrid :: Config -> [Bool] -> Grid
mkTestGrid cfg = listArray ((0,0), (h-1, w-1)) 
  where
    w = width cfg
    h = height cfg
