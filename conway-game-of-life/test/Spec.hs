module Main (main) where
import Test.Hspec (hspec)
import qualified CoreSpec
import qualified RenderSpec 
import qualified PatternsSpec 

main :: IO ()
main = hspec $ do
    CoreSpec.spec
    RenderSpec.spec 
    PatternsSpec.spec 
