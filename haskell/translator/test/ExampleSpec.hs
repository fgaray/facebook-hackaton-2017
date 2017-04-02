module ExampleSpec where

import Prelude hiding (sum)
import Test.Hspec
import Test.QuickCheck



spec :: Spec
spec = do
    describe "example" $ do
        describe "(+)" $ do
            it "sum 2 + 2" $ do
                (2 `sum` 2) `shouldSatisfy` (==4)
            it "sum two arbitrary numbers" $ property $ do
                \(x, y) -> sum x y == x + y
                
                


sum :: Int -> Int -> Int
sum = (+)
