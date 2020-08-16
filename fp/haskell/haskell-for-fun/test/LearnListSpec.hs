module LearnListSpec where

import Test.Hspec
import LearnList
import Data.List

spec :: Spec
spec = do
    describe "intersperse'" $ do
        it "should return [1,0,2]" $
            intersperse' 0 [1,2] `shouldBe` [1,0,2]
    
    describe "intercalate'" $ do
        it "should return [1,1,0,0,2,2]" $
            intercalate' [0,0] [[1,1],[2,2]] `shouldBe` [1,1,0,0,2,2]
    
    describe "transpose'" $ do
        it "should transpose" $
            (transpose' $ transpose' [[1,2,3],[4,5,6],[7,8,9]] )`shouldBe` [[1,2,3],[4,5,6],[7,8,9]]
    
    describe "numUniques'" $ do
        it "should remove duplicated elements and count" $
            numUniques' [1,2,3,1,1] `shouldBe` 3
    
    describe "concat' and concatMap'" $ do
        it "should concat [[1,2],[2,3]] to [1,2,2,3]" $
            concat' [[1,2],[2,3]] `shouldBe` [1,2,2,3]
        it "should concatMap return AABBCC " $
            concatMap' (replicate 2) "ABC" `shouldBe` "AABBCC"

    describe "logical function" $ do
        describe "and'" $ do
            it "should return true" $ do
                and' [True,True] `shouldBe` True
                and' [] `shouldBe` True
            it "should return False" $
                and' [False] `shouldBe` False
        describe "or'" $ do
            it "should return true" $
                or' [True,False] `shouldBe` True
            it "should return false" $ do
                or' [False] `shouldBe` False
                or' [] `shouldBe` False
        describe "any'" $
            it "basic" $ do
                any (==4) [2,3,5,6,1,4] `shouldBe` True
                any (`elem` ['A'..'Z']) "HEYGUYSwhatsup" `shouldBe` True
        describe "all'" $
            it "basic" $ do
                all (>4) [6,9,10]  `shouldBe` True
                all (`elem` ['A'..'Z']) "HEYGUYSwhatsup" `shouldBe` False
    
    describe "take and iterate" $
        it "take infinite iterations n" $
            (take 10 $ iterate (*2) 1) `shouldBe` [1,2,4,8,16,32,64,128,256,512]
    describe "splitAt" $
        it "should split at n index" $
            splitAt 3 "heyman" `shouldBe` ("hey","man")
    describe "takeWhile" $ 
        it "should take util not match" $
            takeWhile (/=' ') "This is a sentence" `shouldBe` "This"
    describe "dropWhile" $
        it "should drop util not match" $
            dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1] `shouldBe` [3,4,5,4,3,2,1]
    describe "span & break" $ do
        it "should split list where first be False" $
            (span (/=4) [1,2,3,4,5,6,7]) `shouldBe` ([1,2,3],[4,5,6,7])
        it "should split list where first be True" $
            (break (==4) [1,2,3,4,5,6,7]) `shouldBe` ([1,2,3],[4,5,6,7])
    describe "sort & group" $ do
        it "should sort a list" $
            (sort [8,5,3,2,1,6,4,2]) `shouldBe` [1,2,2,3,4,5,6,8]
        it "should group a list" $
            (group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7])  `shouldBe` [[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]
    describe "search'" $
        it "should found correct substrings" $ do
            (search' "cat" "im a cat burglar") `shouldBe` True
            (search' "Cat" "im a cat burglar") `shouldBe` False
    describe "lines & words & unlines & unwords" $ do
        it "should split by \"\\\n\" or \"\\\r\\\n\"" $
            (lines "first line\nsecond line\nthird line") 
            `shouldBe` ["first line","second line","third line"]
        it "should split by space" $
            (words "hey these are the words in this sentence") `shouldBe`
            ["hey","these","are","the","words","in","this","sentence"]
        it "should join by space" $
            (unwords ["hey","there","mate"]) `shouldBe` "hey there mate"
    describe "nub & delete" $ do
        it "should deduplicate" $
            (nub [1,2,1]) `shouldBe` [1,2]
        it "should delete first matching" $
            (delete 'h' "abhabh") `shouldBe` "ababh"
    describe "\\ & union & intersect & insert" $ do
        it "should get subsect" $
            ([1..10] \\ [2,5,9]) `shouldBe` [1,3,4,6,7,8,10]
        it "should get union" $
            ("hey man" `union` "man what's up") `shouldBe` "hey manwt'sup"
        it "should get intersect" $
            ([1..7] `intersect` [5..10]) `shouldBe` [5,6,7]
        it "should insert before first bigger" $
            (insert 3 [1,2,4,3,2,1]) `shouldBe` [1,2,3,4,3,2,1]
    describe "groupBy" $
        it "should group by value" $ do
            let values = [-4.3,-2.4,-1.2,0.4,2.3,5.9,10.5,29.1,5.3,-2.4,-14.5,2.9,2.3]
            (groupBy (\x y -> (x > 0) == (y > 0)) values) `shouldBe` [[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]
        
