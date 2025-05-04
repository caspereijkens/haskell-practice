module PatternsSpec (spec) where
import Test.Hspec
import GameOfLife.Patterns
import GameOfLife.Types
import Data.Array (listArray, (!))
import TestUtils (toListGrid)

spec :: Spec
spec = describe "Pattern Insertion" $ do
    let standardCfg = Config 5 5 False

    it "Wraps pattern correctly" $ do
        let cfg = Config 5 5 True 
            pattern = listArray ((0,0),(0,0)) [True]  -- Single-cell pattern
            grid = insertPatternAt cfg (5,5) pattern (mkGrid cfg)
        grid ! (0,0) `shouldBe` True  -- Check wrapped position

    it "Inserts glider correctly" $
        let result = insertPatternAt standardCfg (0,0) glider (mkGrid standardCfg)
            expected = [ [False,True,False,False,False]
                      , [False,False,True,False,False]
                      , [True,True,True,False,False]
                      , [False,False,False,False,False]
                      , [False,False,False,False,False] ]
        in toListGrid result `shouldBe` expected
