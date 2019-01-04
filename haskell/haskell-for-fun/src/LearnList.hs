module LearnList where

import Data.List
import MyUtil

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- all: :m Data.List Data.Map Data.Set
-- select: import Data.List (nub,sort)
-- hiding: import Data.List hiding (nub)
-- special: import qualified Data.Map
-- alias: import Data.Map as M

intersperse' :: a -> [a] -> [a]
intersperse' y (x:[]) = [x]
intersperse' y (x:xs) = x:y:(intersperse' y xs)

intercalate' :: [a] -> [[a]] -> [a]
intercalate' y (x:[]) = x
intercalate' y (x:xs) = x++y++(intercalate' y xs)

testTranspose :: [[Char]]
testTranspose = transpose ["hey","there","guys"]

testSth :: IO ()
testSth = do 
    putStrLn $ concat ["abc","def"]
    putStrLn $ concatMap (replicate 3) "ABC"
    putStrLn $ show $ and $ map (==4) [1,2,3,4]