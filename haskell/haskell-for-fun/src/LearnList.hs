module LearnList where

import           Data.List
import           Data.Bool
import           MyUtil

numUniques' :: (Eq a) => [a] -> Int
numUniques' = length . nub

-- all: :m Data.List Data.Map Data.Set
-- select: import Data.List (nub,sort)
-- hiding: import Data.List hiding (nub)
-- special: import qualified Data.Map
-- alias: import Data.Map as M

intersperse' :: a -> [a] -> [a]
intersperse' y (x : []) = [x]
intersperse' y (x : xs) = x : y : (intersperse' y xs)

intercalate' :: [a] -> [[a]] -> [a]
intercalate' y (x : []) = x
intercalate' y (x : xs) = x ++ y ++ (intercalate' y xs)

transpose' :: [[a]] -> [[a]]
transpose' []         = []
transpose' ([] : xss) = transpose' xss
transpose' ((x : xs) : xss) =
  (x : [ h | (h : _) <- xss ]) : transpose' (xs : [ t | (_ : t) <- xss ])

concat' :: [[a]] -> [a]
concat' xs = foldl (\acc x -> acc ++ x) [] xs

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f xs = concat $ map f xs

all' :: (a -> Bool) -> [a] -> Bool
all' f []       = True
all' f (x : xs) = if f x then all' f xs else False

any' :: (a -> Bool) -> [a] -> Bool
any' f []       = False
any' f (x : xs) = if not $ f x then any' f xs else True

and' :: [Bool] -> Bool
and' xs = all' id xs

or' :: [Bool] -> Bool
or' xs = any' id xs

search' :: (Eq a) => [a] -> [a] -> Bool
search' needle haystack =
  let nlen = length needle
  in  foldl (\acc x -> if take nlen x == needle then True else acc)
            False
            (tails haystack)

-- similar to isInfixOf

isPrefixOf' :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf' target source = take (length target) source == target

isSuffixOf' :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf' target source =
  drop (length source - length target) source == target

elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = any' (== x) xs

notElem' :: (Eq a) => a -> [a] -> Bool
notElem' x xs = not $ elem' x xs

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p xs = (filter (p) xs, filter (not . p) xs)

enumerate' :: [a] -> [(a, Int)]
enumerate' xs = zip xs [0 ..]

findIndex' :: (a -> Bool) -> [a] -> Maybe Int
findIndex' f xs = findIndex'' f xs 0
 where
  findIndex'' :: (a -> Bool) -> [a] -> Int -> Maybe Int
  findIndex'' f (y : ys) index =
    let p = f y
    in  if p
          then Just index
          else if null ys then Nothing else findIndex'' f ys (index + 1)

elemIndex' :: (Eq a) => a -> [a] -> Maybe Int
elemIndex' x xs = findIndex' (== x) xs

findIndices' :: (a -> Bool) -> [a] -> [Int]
findIndices' f xs = findIndices'' f xs 0
 where
  findIndices'' :: (a -> Bool) -> [a] -> Int -> [Int]
  findIndices'' f (y : ys) index =
    let p = f y
    in  if p
          then index : (findIndices'' f ys (index + 1))
          else if null ys then [] else findIndices'' f ys (index + 1)

elemIndices'' :: (Eq a) => a -> [a] -> [Int]
elemIndices'' x xs = findIndices' (== x) xs

-- xxBy = xx (==)

