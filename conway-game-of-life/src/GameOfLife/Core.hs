module GameOfLife.Core
    ( nextGen
    , countNeighbors
    ) where

import GameOfLife.Types
import Data.Array (listArray, (!), bounds, range)

nextGen :: Config -> Grid -> Grid
nextGen cfg grid = listArray (bounds grid) newCells
  where
    newCells = map (calcNextCell grid) (range (bounds grid))
    
    calcNextCell :: Grid -> Position -> Bool
    calcNextCell g pos =
      let alive = g ! pos
          n = countNeighbors cfg g pos
      in case (alive, n) of
           (True, 2) -> True
           (_,    3) -> True
           _         -> False

countNeighbors :: Config -> Grid -> Position -> Int
countNeighbors cfg grid (y, x) = 
  length $ filter (grid !) neighborPositions
  where
    neighborPositions = filter inBounds
      [ (wrapY (y + dy), wrapX (x + dx))
      | dy <- [-1..1]
      , dx <- [-1..1]
      , not (dy == 0 && dx == 0) ]

    wrapX coord
      | wrapEdges cfg = ((coord - minX) `mod` width) + minX
      | otherwise     = coord
    wrapY coord
      | wrapEdges cfg = ((coord - minY) `mod` height) + minY
      | otherwise     = coord

    inBounds (y', x') = y' >= minY && y' <= maxY && x' >= minX && x' <= maxX

    ((minY, minX), (maxY, maxX)) = bounds grid
    height = maxY - minY + 1
    width  = maxX - minX + 1
