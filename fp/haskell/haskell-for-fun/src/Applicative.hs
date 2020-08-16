module Applicative where

import Control.Applicative
import MyUtil

{--
:t fmap (*3)
fmap (*3) :: (Num a, Functor f) => f a -> f a
:t fmap (replicate 3)
fmap (replicate 3) :: (Functor f) => f a -> f [a]
--}

{--
rules 
-- 1
fmap match id
-- 2
fmap match fmap (f.g) = fmap f . fmap g
--}

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)

-- CMaybe doesn't obey applicative

{--
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a->b) -> f a -> f b

instance Applicative Maybe where
    pure :: Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something

-- use
Just (+3) <*> Just 9 = Just 12

-- applicative style
pure (*) <*> Just 3 <*> Just 9
Just 12

(<$>) :: (Functor f) => (a->b) -> f a -> f b
f <$> x = fmap f x

instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x|f<-fs,x<-xs]

instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return $ f x

instance Applicative ((->)r) where
    pure x = (\_->x)
    f <*> g = \x -> f x (g x)

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (ZipWith (\f x-> f x) fs xs)

getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"

--}

mySequenceA :: (Applicative f) => [f a] -> f [a]
mySequenceA [] = pure []
mySequenceA (x:xs) = (:) <$> x <*> mySequenceA xs

anotherSequenceA :: (Applicative f) => [f a] -> f [a]
anotherSequenceA = foldr (liftA2 (:)) (pure [])

tryLiftA :: IO()
tryLiftA = do
    learn $ liftA2 (:) (Just 3) (Just [4])
    learn $ mySequenceA [Just 1,Just 2]
    learn $ anotherSequenceA [(+3),(+2),(+1)] 3