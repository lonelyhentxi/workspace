module InputAndOutput where

import Text.Printf
import Control.Monad
import Data.Char

-- hello world

helloWorld :: IO()
helloWorld = putStrLn "Hello, world"

multilineIO :: IO()
multilineIO  = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ printf "Hey %s, your rock!" name

{--
:t getLine
getLine :: IO String

_ <- putStrLn "BLAH"
because of its return value is (), can be simplified as putStrLn "BLAH"
and the last IO action can not bind anything

only if you use main/<-/ghci, it can auto show in your monitor
--}

reverseWords :: String -> String
reverseWords = unwords .  map reverse . words

doReadLines :: IO()
doReadLines = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            doReadLines

-- in haskell, return will wrap some pure value as a IO action

putStr' :: String -> IO()
putStr' [] = return ()

tryWhen :: IO()
tryWhen = do
    c <- getChar
    when (c /= ' ' ) $ do
        putChar c
        tryWhen


trySequence :: IO [()]
trySequence = sequence (map print [1,2,3,4])

tryForever :: IO ()
tryForever = forever $ do
    putStr "Give me some input:"
    l <- getLine
    putStrLn $ map toUpper l

tryForM :: IO [()]
tryForM = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ printf "Which color do you associate with the number %s ?\n" (show a)
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1,2,3 and 4 are:"
    mapM putStrLn colors

{-- file and charstream --}

tryGetContents :: IO ()
tryGetContents = do
    contents <- getContents
    putStr (map toUpper contents)
