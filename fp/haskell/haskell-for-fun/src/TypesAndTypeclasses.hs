module TypesAndTypeclasses where

import           MyUtil
import qualified Data.Map.Strict as Map

data Point = Point Float Float deriving (Show, Eq, Read)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show, Eq, Read)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) =
    (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r

data Person = Person {
    firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
} deriving (Show, Eq, Read)

data Car = Car {
    company :: String,
    model :: String,
    year :: Int
} deriving (Show, Eq, Read)

data MyMaybe a = MyNothing | MyJust a

-- Derived instances
-- Eq used in == and /=
-- Show used to convert to string, Read used to read from string
-- Ord first check pattern matching then check value

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Because of bound and ord, can use minBound
-- Because of enum bound and ord

-- Type synonyms/alias

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name phoneNumber phoneBook = (name,phoneNumber) `elem` phoneBook

type AssocList k v = [(k v)]
type IntMap v = Map.Map Int v

data MyEither a b = MyLeft a | MyRight b deriving (Eq, Ord, Read, Show)

-- Error in left
-- Success in right

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> MyEither String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> MyLeft $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
        then MyRight code
        else MyLeft $ "Locker " ++ show lockerNumber ++ " is already taken!"

-- Recursive data structures

data MyList a = MyListEmpty | a :-: ( MyList a) deriving (Show, Read, Eq, Ord)

infixr 5 :-:

data MyBinarySearchTree a = MyBinarySearchTreeEmpty | MyBinarySearchTreeNode a (MyBinarySearchTree a) (MyBinarySearchTree a) deriving (Show, Read, Eq)

singletonTree :: a -> MyBinarySearchTree a
singletonTree x = MyBinarySearchTreeNode x MyBinarySearchTreeEmpty MyBinarySearchTreeEmpty

treeInsert :: (Ord a) => a -> MyBinarySearchTree a -> MyBinarySearchTree a
treeInsert x MyBinarySearchTreeEmpty = singletonTree x
treeInsert x (MyBinarySearchTreeNode a left right)
        | x==a = MyBinarySearchTreeNode x left right
        | x<a = MyBinarySearchTreeNode a (treeInsert x left) right
        | x>a = MyBinarySearchTreeNode a left (treeInsert x right)

treeElem :: (Ord a) => a -> MyBinarySearchTree a -> Bool
treeElem x MyBinarySearchTreeEmpty = False
treeElem x (MyBinarySearchTreeNode a left right)
        | x == a = True
        | x<a = treeElem x left
        | x>a = treeElem x right

-- infixr right-associative
-- infixl left-associative
-- first param show its priority; * 7, + 6
-- to use function as pattern matching, it should be a constructor

-- Typeclasses second class

{-- class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x/=y)
    x /= y = not (x==y)
    -- Just like a trait in rust    
--}

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- use class in defining new typeclass, and use instance in defining a instance of a certain typeclass

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- deriving Show will use constructor name as output string

{--
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing === Nothing = True
    _ == _ = False
--}

-- in ghci, can use :info to show typeclass information

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = 
    if yesno yesnoVal then yesResult else noResult

-- functor typeclass

class MyFunctor f where
    myFunctorMap :: (a->b) -> f a -> f b

-- the typeclass has no default implement

instance MyFunctor [] where
    myFunctorMap = map

instance MyFunctor Maybe where
    myFunctorMap f (Just x) = Just $ f x
    myFunctorMap f Nothing = Nothing

-- Functor recieve a constructor rather than a detailed class

instance MyFunctor MyBinarySearchTree where
    myFunctorMap f MyBinarySearchTreeEmpty = MyBinarySearchTreeEmpty
    myFunctorMap f (MyBinarySearchTreeNode x left right)
     = MyBinarySearchTreeNode (f x) (myFunctorMap f left) (myFunctorMap f right)

-- functor should obey some laws such as sorts and so on

-- Kind
-- in ghci, use :k type
{-- code
ghci> :k Int
Int :: *
-- codeend
* as detailed type
-- code start
> :k Maybe
Maybe :: * -> *
> :k Either String
Either String Int :: *
-- code end
--}

class Tofu t where
    tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving (Show)




script :: IO ()
script = do
    learn $ Car { company = "Ford", model = "Mustang", year = 1967 }
    learn (read "Car {company = \"Tesla\", model = \"car\", year = 2018}" :: Car)
    learn (minBound :: Day)
    learn ([minBound .. maxBound] :: [Day])
    learn [Thursday .. Sunday]