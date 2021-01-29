module MyUtil where

learn :: Show a => a -> IO()
learn sth = putStrLn $ show sth