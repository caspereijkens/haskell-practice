module RenderSpec (spec) where
import Test.Hspec
import GameOfLife.Render
import Data.Array (listArray)

spec :: Spec
spec = describe "Grid Rendering" $ do
    it "Renders live cells as '■'" $
        renderCell True `shouldBe` '■'
    
    it "Renders dead cells as ' '" $
        renderCell False `shouldBe` ' '
    
    it "Renders 2x2 grid correctly" $
        let grid = listArray ((0,0),(1,1)) [True, False, False, True]
        in render grid `shouldBe` "■ \n ■\n"
