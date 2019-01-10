module InputAndOutput where

import Text.Printf
import Control.Monad
import Data.Char
import System.IO
import System.Random
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

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

convertIO :: IO ()
convertIO = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line->length line < 10) . lines

-- file io

{--
openFile :: FilePath -> IOMode -> IO Handle
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
type FilePath = String
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
--}

tryWithFile :: IO ()
tryWithFile = do 
    withFile "../resource/io.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStrLn contents
        )

-- random
{--
random :: (RandomGen g, Random a) => g -> (a,g)
should get new random gen from output, otherwise yield same results
--}

validateCoins :: StdGen -> Int -> Bool
validateCoins gen times =
    if times == 0 then True
    else case random gen of
        (state,newGen) -> validateCoinsRec newGen state (times-1)
        where validateCoinsRec :: StdGen -> Bool -> Int -> Bool
              validateCoinsRec gen state times =
                if times == 0 then True
                else
                    let (res,newGen) = random gen
                    in if res==state then validateCoinsRec newGen state (times-1)
                    else False

validateCoins' :: StdGen -> Int -> Bool
validateCoins' gen times =
    if times == 0 then True
    else case random gen of
        (state,_) -> validateCoinsRec' gen state (times-1)
        where validateCoinsRec' :: StdGen -> Bool -> Int -> Bool
              validateCoinsRec' gen state times =
                if times == 0 then True
                else
                    let (res,_) = random gen
                    in if res==state then validateCoinsRec' gen state (times-1)
                    else False

{--
bytestring
--}

tryByteStringSimpleUse :: IO()
tryByteStringSimpleUse = do
    putStrLn $ show $ B.pack [99,97,110]
    putStrLn $ show $ B.unpack $ B.pack [99,97,110]
