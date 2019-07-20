module LearnSet where

import qualified Data.Set as Set
import Data.List
import qualified Data.Map as Map
import Data.Char
import Data.Function
import MyUtil

script :: IO()
script = 
    let wtf = Set.fromList "wtf"
        tfboy = Set.fromList "tfboy" 
    in 
        do
        learn $ Set.fromList "wtf"
        learn $ Set.difference wtf tfboy
        learn $ Set.difference tfboy wtf
        learn $ Set.union tfboy wtf
        learn $ Set.map (\_->1) tfboy