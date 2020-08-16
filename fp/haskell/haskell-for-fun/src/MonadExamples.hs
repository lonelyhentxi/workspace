module MonadExamples where

import Control.Monad
import MyUtil

-- (>>=) :: (Monad m) => m a -> (a->m b) -> m b
-- (<*>) :: (Applicative f) => f (a->b) -> f a -> f b

applyMaybe :: Maybe a -> ( a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

tryApplyMaybe :: IO()
tryApplyMaybe = do
    learn $ Just 3 `applyMaybe` \x -> Just (x+1)
    learn $ Nothing `applyMaybe` \x -> Just (x++ ":)")

-- monad typeclass
{--

class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a->m a) -> m b
    (>>) :: m a -> m b -> m b
    x >> y = x >>= \_ -> y
    fail :: String -> m a
    fail msg  = error msg

instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
    fail _ = Nothing
--}

tryMaybeMonad :: IO()
tryMaybeMonad = do
    learn ( return "WHAT" :: Maybe String )
    learn ( Just 9 >>= \x -> return (x*10) )
    learn ( Nothing >>= \x -> return (x*10) )

type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (l,r) = (l+n,r)

landRight :: Birds -> Pole -> Pole
landRight n (l,r) = (l,r+n)

(-:) :: a -> (a->b) -> b
x -: f = f x

tryFrontParam :: IO()
tryFrontParam = do
    learn $ 100 -: (*3)
    learn $ True -: not
    learn $ (0,0) -: landLeft 2
    learn $ (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2

landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (l,r)
    | abs ((l+n)-r) < 4 = Just (l+n,r)
    | otherwise = Nothing

landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (l,r)
    | abs (l-(r+n)) < 4 = Just (l,r+n)
    | otherwise = Nothing

banana' :: Pole -> Maybe Pole
banana' _ = Nothing

tryNewLand :: IO ()
tryNewLand = do
    let step1 = return (0,0) >>= landRight' 2 >>= landLeft' 2 >>= landRight' 2
        step2 = step1 >> (Nothing :: Maybe Pole)
        in 
            do  learn $ step1
                learn $ step1 >>= banana'
                learn $ step2

foo :: Maybe String
foo = Just 3 >>= (\x ->
      Just "!" >>= (\y->
      Just (show x ++ y)))

marySue :: Maybe Bool
marySue = do
    x <- Just 9
    Just (x>9)

routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft' 2 start
    second <- landRight' 2 first
    landLeft' 1 second

-- you can use pattern matching in do notation as well

justH :: Maybe Char
justH = do
    (x:xs) <- Just "Hello"
    return x

-- if failed in pattern matching in do notation, monads usually define their own fail function
{--
fail _ = Nothing
-- for Maybe x
--}

{--
list monad
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []
--}

tryListMonad :: IO()
tryListMonad = do
    learn $ [3,4,5] >>= \x -> [x,-x]
    learn $ [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)

listOfTuples :: [(Int,Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a','b']
    return (n,ch)

-- do notation and list comprehenshion
-- The list comprehenshion is the syntactic sugar of the list monad.

-- MonadPlus and guard function

{--

class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

-- MonadPlus is used to represent monads with monoid behaviour.

instance MonadPlus [] where
    mzero = []
    mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero
--}

tryGuardFunction :: IO()
tryGuardFunction = learn $ [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)

-- Horse exploration

type KnightPos = (Int,Int)
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),
        (c+1,r+2),(c+1,r-2),(c-1,r-2),(c-1,r+2)]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-- the law of monad
{--
-- 1. return x>>=f should equal to f x
-- 2. m >>= return should equal to m
-- 3. (m >>= f) >>= g should equal to (m>>=(\x->f x>>=g))
--}