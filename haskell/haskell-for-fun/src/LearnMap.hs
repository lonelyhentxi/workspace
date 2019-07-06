module LearnMap where

import qualified Data.Map.Strict as Map
import Data.List
import Data.Char
import Data.Function
import MyUtil

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc->Map.insert k v acc) Map.empty

singleton' :: (Ord k) => k -> v -> Map.Map k v
singleton' a b = Map.insert a b Map.empty

phoneBookToMap :: (Ord k) => [(k,String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++number2) xs

script :: IO()
script = do
    learn $ Map.fromList [(1,2),(3,4),(5,6),(7,8)]
    learn $ Map.insert 3 100 Map.empty
    learn $ Map.null Map.empty
    learn $ Map.size Map.empty
    learn $ Map.singleton 1 'a'
    let testMap = Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')] in do
        learn $ Map.lookup (digitToInt '1') testMap
        learn $ Map.member 1 testMap
        learn $ Map.toList $ Map.map (intToDigit . (mod 5) . (+ 3). ord) testMap
        learn $ Map.filter isUpper $ testMap
        learn $ Map.toList $ testMap
        learn $ Map.keys testMap
        learn $ Map.elems testMap
        learn $ phoneBookToMap [("a","123"),("a","234"),("b","cde")]
        learn $ Map.fromListWith (++) [("a","123"),("a","234"),("b","cde")]
        learn $ Map.toList $ Map.insertWith (\x y -> if ((>) `on` ord) x y then x else y) 2 'B' testMap