module GameOfLife.Render
    ( render
    , renderCell
    ) where
import GameOfLife.Types
import Data.Array (bounds, (!))

render :: Grid -> String
render grid = unlines [ [ renderCell (grid ! (y,x)) | x <- [minX..maxX] ]
                         | y <- [minY..maxY] ]
  where
    ((minY,minX), (maxY,maxX)) = bounds grid

renderCell :: Bool -> Char  -- Changed from String to Char
renderCell True = '■'
renderCell False = ' '
