module LearnZipper where

import MyUtil
import Data.List (break)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree = 
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a,Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (r, (RightCrumb x l):bs)

goRight :: (Tree a,Breadcrumbs a) -> (Tree a,Breadcrumbs a)
goRight (Node x l r, bs) = (l, (RightCrumb x l):bs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a,Breadcrumbs a)
goUp (t,LeftCrumb x r:bs) = (Node x t r, bs)
goUp (t,RightCrumb x l:bs) = (Node x l t, bs)

type Zipper a = (Tree a, Breadcrumbs a)

modify :: (a->a) -> Zipper a->Zipper a
modify f (Node x l r, bs) = (Node (f x) l r,bs)
modify f (Empty,bs) = (Empty, bs)

(-:) :: a -> (a->b) -> b
x -: f = f x

tryMoveFocus :: IO()
tryMoveFocus = do
    let newFocus = (freeTree,[]) -: goLeft -: goRight -: modify (\_ -> 'P')
        newFocus2 = newFocus -: goUp -: modify (\_ -> 'X')
        in do
            learn $ newFocus
            learn $ newFocus2

topMost :: Zipper a -> Zipper a
topMost (t,[]) = (t,[])
topMost z = topMost $ goUp z

type ListZipper a = ([a],[a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs,bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)

-- start simple filesystem

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk = 
    Folder "root"
        [
            File "a" "aaa",
            File "b" "bbb",
            Folder "c" []
        ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item: rs) = break (nameIs name) items
        in (item, FSCrumb folderName ls rs: bs)
    
nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

tryVirtualFS :: IO()
tryVirtualFS = let newFocus = (myDisk, []) -: fsTo "c" -: fsUp in learn $ newFocus

-- be careful

goLeft' :: Zipper a -> Maybe (Zipper a)
goLeft' (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft' (Empty, _) = Nothing

goRight' :: Zipper a -> Maybe (Zipper a)
goRight' (Node x l r,bs) = Just (r, RightCrumb x l:bs)

goUp' :: Zipper a -> Maybe (Zipper a)
goUp' (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp' (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp' (_, []) = Nothing

trySafeCrumb :: IO()
trySafeCrumb = learn $ return (freeTree,[]) >>= goLeft' >>= goRight'