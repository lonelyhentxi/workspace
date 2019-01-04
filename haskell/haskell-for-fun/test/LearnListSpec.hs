module LearnListSpec where

import Test.Hspec
import LearnList

spec :: Spec
spec = do
    describe "intersperse'" $ do
        it "should return [1,0,2]" $
            do intersperse' 0 [1,2] `shouldBe` [1,0,2]
    
    describe "intercalate'" $ do
        it "should return [1,1,0,0,2,2]" $
            do intercalate' [0,0] [[1,1],[2,2]] `shouldBe` [1,1,0,0,2,2]