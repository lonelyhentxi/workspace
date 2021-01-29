module LearnChar where

import Data.Function
import Data.List
import Data.Char
import MyUtil

words' :: String -> [String]
words' str =  filter (not . isSpace . head) . groupBy ((==) `on` isSpace) $ str

encode :: Int -> String -> String
encode shift msg = 
    let ords = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

script :: IO()
script = do
    learn $ all isAlpha "bobby283"
    learn $ all isAlphaNum "eddy the fish!"
    learn $ words' "hey guys its me"
    learn $ generalCategory ' '
    learn $ map (intToDigit . digitToInt) "FF85AA"
    learn $ decode 5 . encode 5 $ "This is a sentence"
