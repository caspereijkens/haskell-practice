module CoreSpec (spec) where
import Test.Hspec
import Test.QuickCheck
import GameOfLife.Core
import GameOfLife.Types 
import Data.Array (bounds)
import TestUtils (toListGrid, mkTestGrid)

spec :: Spec
spec = describe "Game of Life Rules" $ do
    let standardCfg = Config 3 3 False
        -- Helper specific to 3x3 grids in this test file
        mk3x3Grid = mkTestGrid standardCfg

    it "Normal 3x3 grid behavior" $ do
        let testGrid = mk3x3Grid
              [ True,  True,  True
              , True,  False, True
              , True,  True,  True ]
            expected = [[True, False, True]
                      ,[False, False, False]
                      ,[True, False, True]]
        toListGrid (nextGen standardCfg testGrid) `shouldBe` expected

    it "Dead cell with 3 neighbors becomes alive" $ do
        let deadGrid = mk3x3Grid
              [ False, False, False
              , True,  False, True
              , False, True,  False ]
            expected = [[False, False, False]
                       ,[False, True,  False]
                       ,[False, True, False]]
        toListGrid (nextGen standardCfg deadGrid) `shouldBe` expected
    
    it "Empty grid stays empty" $ do
        let emptyGrid = mkGrid standardCfg
        nextGen standardCfg emptyGrid `shouldBe` emptyGrid
    
    it "Handles edge cases when wrapEdges=False" $ do
        let edgeGrid = mk3x3Grid
              [ True,  False, False
              , False, False, False
              , False, False, False ]
            expected = [[False, False, False]
                      ,[False, False, False]
                      ,[False, False, False]]
        toListGrid (nextGen standardCfg edgeGrid) `shouldBe` expected

    it "Always returns same-sized grid" $ do
        property $ \(Positive w) (Positive h) -> 
            let cfg = Config w h False
                grid = mkGrid cfg
                result = nextGen cfg grid
                ((minY,minX),(maxY,maxX)) = bounds result
            in maxY - minY + 1 == h && maxX - minX + 1 == w
