module MoreMonad where

import           Data.Monoid
import           MyUtil
import           Control.Monad.Writer
import           Control.Monad.State
import           System.Random
import           Data.List
import           Data.Ratio

applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _       = ("beer", Sum 30)

tryAddDrink :: IO ()
tryAddDrink = do
    learn $ ("beans", Sum 10) `applyLog` addDrink
    learn $ ("dogmeat", Sum 5) `applyLog` addDrink
    learn $ ("dogmeat", Sum 5) `applyLog` addDrink

-- Writer typeclass


{--



newtype Writer w a = Writer { runWriter :: (a,w) }



instance (Monoid w) => Monad (Writer w) where

    return x = Writer (x,mempty)

    (Writer (x,v)) >>= f = let (Writer (y,v')) = f x in Writer (y, v `mappend` v')



--}

tryWriter :: IO ()
tryWriter = do
    learn $ runWriter (return 3 :: Writer String Int)
    learn $ runWriter (return 3 :: Writer (Sum Int) Int)
    learn $ runWriter (return 3 :: Writer (Product Int) Int)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a * b)

-- use differential list


newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
    DiffList f <> DiffList g = DiffList (\xs -> f (g xs))

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)

tryDiffList :: IO ()
tryDiffList = do
    learn $ fromDiffList
        (toDiffList [1, 2, 3, 4] `mappend` toDiffList [1, 2, 3])

gcd' :: Int -> Int -> Writer (DiffList String) Int
gcd' a b
    | b == 0 = do
        tell (toDiffList ["Finish with " ++ show a])
        return a
    | otherwise = do
        result <- gcd' b (a `mod` b)
        tell
            (toDiffList
                [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
            )
        return result

tryGcd' :: IO ()
tryGcd' = do
    mapM_ putStrLn . fromDiffList . snd . runWriter $ gcd' 110 34

-- Reader


{--

instance Monad ((->)r) where

    return x = \_ -> x

    h >>= f = \w -> f (h w) w

--}

-- Reader monad


addStuff :: Int -> Int
addStuff = do
    a <- (* 2)
    b <- (+ 10)
    return (a + b)

tryAddStuff :: IO ()
tryAddStuff = learn $ addStuff 1

-- better representation with state computation


type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x : xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a : xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack =
    let ((), newStack1) = push 3 stack
        (a , newStack2) = pop newStack1
    in  pop newStack2

pop' :: State Stack Int
pop' = state $ \(x : xs) -> (x, xs)

push' :: Int -> State Stack ()
push' a = state $ \xs -> ((), a : xs)

stackManip' :: State Stack Int
stackManip' = do
    push' 3
    a <- pop'
    pop'

tryStackManip :: IO ()
tryStackManip = learn $ runState stackManip' [5, 8, 2, 1]

get' :: MonadState a m => m a
get' = state $ \s -> (s, s)

put' :: MonadState a m => a -> m ()
put' newState = state $ \s -> ((), newState)

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

oneCoins :: State StdGen (Bool)
oneCoins = do
    a <- randomSt
    return (a)

{--

instance (Error e) => Monad (Eigher e) where

    return x = Right x

    Right x >>= f = f x

    Left err >>= f = Left err

    fail msg = Left (strMsg msg)

--}

tryMonadError :: IO ()
tryMonadError = do
    learn $ Left "boom" >>= \x -> return (x + 1)
    learn $ (Left "boom" >>= \x -> Left "no way!" :: Either String String)
    learn $ (Right 100 >>= \x -> Left "no way!" :: Either String Int)

liftM' :: (Monad m) => (a -> b) -> m a -> m b
liftM' f m = m >>= (\x -> return (f x))

tryLiftM' :: IO ()
tryLiftM' = do
    learn $ runWriter $ liftM' not $ writer (True, "chickpes")
    learn $ (runState (liftM (+ 100) pop') [1, 2, 3, 4])

ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' mf m = do
    f <- mf
    x <- m
    return (f x)

tryAp' :: IO ()
tryAp' = do
    learn $ Just (+ 3) `ap'` Just 4
    learn $ [(+ 1), (+ 2), (+ 3)] `ap'` [10, 11]

join' :: (Monad m) => m (m a) -> m a
join' mm = do
    x <- mm
    x

filterM' :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM' p =
    foldr (\x -> liftM2 (\flg -> if flg then (x :) else id) (p x)) (return [])

powerset :: [a] -> [[a]]
powerset xs = filterM' (\x -> [True, False]) xs

tryFilterM :: IO ()
tryFilterM = do
    learn $ powerset [1, 2, 3]

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ minit []       = return minit
foldM' f minit (x : xs) = f minit x >>= \fax -> foldM' f fax xs

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
    [(x, "")] -> Just x
    _         -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x : y : ys) "*"          = return ((y * x) : ys)
foldingFunction (x : y : ys) "+"          = return ((y + x) : ys)
foldingFunction (x : y : ys) "-"          = return ((y - x) : ys)
foldingFunction xs numberString = liftM' (: xs) (readMaybe numberString)

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    return result

trySolveRPN :: IO ()
trySolveRPN = do
    learn $ solveRPN "1 2 * 4 +"
    learn $ solveRPN "1 8 foo"

tryMonadFolding :: IO ()
tryMonadFolding = learn $ let f = foldr (.) id [(+ 1), (* 100), (+ 1)] in f 1

-- create monad

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving (Show)

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

tryProb :: IO ()
tryProb = learn $ fmap negate (Prob [(3, 1 % 2), (5, 1 % 4), (9, 1 % 4)])
