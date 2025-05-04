module GameOfLife.Types
    ( Config(..)  -- Export Config and its fields
    , Grid
    , mkGrid
    , Position
    ) where
import Data.Array (Array, listArray)

type Position = (Int, Int)
type Grid = Array Position Bool

data Config = Config
  { width :: Int
  , height :: Int
  , wrapEdges :: Bool
  } deriving (Show)

mkGrid :: Config -> Grid
mkGrid cfg = listArray ((0,0), (height cfg - 1, width cfg - 1)) (repeat False)
