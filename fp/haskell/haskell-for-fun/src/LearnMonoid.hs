module LearnMonoid where

import MyUtil
import Data.Monoid
import Data.Foldable as F

-- wrapped with newtype

newtype MyCharList = MyCharList { getMyCharList :: [Char] } deriving (Show,Eq)

tryCharList :: IO()
tryCharList = do
    learn $ MyCharList "This will be shown!"

newtype MyPair b a = MyPair { getMyPair :: (a, b) }

instance Functor (MyPair c) where
    fmap f (MyPair (x, y)) = MyPair (f x, y)

tryMyPairFunctor :: IO()
tryMyPairFunctor = do
    learn $ getMyPair $ fmap (*100) (MyPair (2,3))

data DataCoolBool = DataCoolBool { getDataCoolBool :: Bool }
newtype NewTypeCoolBool = NewTypeCoolBool { getNewTypeCoolBool :: Bool }

helloMeData :: DataCoolBool -> String
helloMeData _ = "Hello Me"

helloMeNewType :: NewTypeCoolBool -> String
helloMeNewType _ = "Hello Me"

tryHelloMe :: IO()
tryHelloMe = do
    learn $ helloMeData undefined
    learn $ helloMeNewType undefined

-- haskell for fun tells that helloMeData should throw a exception, but not
-- type IntList = [Int]

{--
about monoid
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty

mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
--}

tryListMonoid :: IO()
tryListMonoid = do
    learn $ [] `mappend` "abc"
    learn $ "abc" `mappend` []
    learn $ ("abc" `mappend` "efg") `mappend` "hij" == "abc" `mappend` ("efg" `mappend` "hij")

{--
newtype MyProduct a = MyProduct a  deriving (Eq, Ord, Num, Show)
newtype MySum a = MySum a deriving (Eq, Ord, Num, Show)

instance Num a => Monoid (MyProduct a) where
    mempty = MyProduct 1
    mappend (MyProduct x) (MyProduct y) = MyProduct $ x * y

instance Num a => Monoid (MySum a) where
    mempty = MySum 0
    mappend (MySum x) (MySum y) = MySum $ x + y
--}

tryProdAndSum :: IO()
tryProdAndSum = do
    learn $ Product 3 `mappend` Product 9
    learn $ Sum 3 `mappend` Sum 9

tryAllAndAny :: IO()
tryAllAndAny = do
    learn $ getAll $ mempty `mappend` All True
    learn $ getAny $ mempty `mappend` Any False

tryOrdering :: IO()
tryOrdering = do
    learn $ LT `mappend` GT
    learn $ EQ `mappend` LT

{--
instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing `mappend` m = m
    m `mappend` Nothing = m
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)
--}

tryFoldable :: IO()
tryFoldable = do
    learn $ F.foldr (+) 2 (Just 9)

{--
foldMap :: (Monoid m, Foldable t) => (a->m) -> t a -> m
--}

data Tree a  = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance F.Foldable Tree where
    foldMap f EmptyTree = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r