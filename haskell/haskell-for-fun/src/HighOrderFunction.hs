module HighOrderFunction where

multThree :: Int -> Int -> Int -> Int
multThree x y z = x*y*z

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

applyTwice :: ( a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a->b->c)->[a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a->b->c)->(b->a->c)
flip' f = g
  where g x y = f y x

largestDivisible :: Integer
largestDivisible = head (filter p [10000,9999..])
  where p x = x `mod` 3829 == 0

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n:chain (n `div` 2)
  | odd n = n:chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x->acc + x) 0 xs

map' :: (a->b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x: acc) [] xs

map'' :: (a->b) -> [a] -> [b]
map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs

highOrderFunctionTest :: IO ()
highOrderFunctionTest = do
    print $ compareWithHundred $ multThree 10 5 2
    print $ divideByTen 200
    print $ applyTwice (3:) [1]
    print $ compare [6,8,7,9] $ zipWith' (+) [4,2,5,6] [2,6,2,3]
    print $ zipWith (flip' div) [2,2..] [10,8,6,4,2]
    print $ takeWhile (/= ' ') "elephants know how to party"
    print $ largestDivisible
    print $ sum $ takeWhile (<10000) (filter odd (map (^2) [1..]))
    print $ sum $ takeWhile (<10000) [m | m <-[n^2|n<-[1..]], odd m]
    print numLongChains
    print $ map (+3) [1,6,3,2]
    print $ zipWith (\a b -> (a*30+3)/b) [5,4,3,2,1] [1,2,3,4,5]
    -- 匹配失败的 lambda 会引发运行时错误
    print $ zipWith (flip'' (++)) ["love you","love me"] ["i ","you "]
    print $ map (flip'' subtract 20) [1,2,3,4]
    print $ sum' [3,5,2,1]
    print $ ( (\x -> foldl (\z y -> max z y) 1 x) [1,2,3] )
    -- 无限列表的折叠
    print $ and' (repeat False)