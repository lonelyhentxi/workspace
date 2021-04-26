module ThreeKyu.Isomorphism where

import Control.Monad
import Data.Void

type ISO a b = (a -> b, b -> a)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd

isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

refl :: ISO a a
refl = (id, id)

symm :: ISO a b -> ISO b a
symm (x, y) = (y, x)


trans :: ISO a b -> ISO b c -> ISO a c
trans (x, x') (y, y') = (y . x, x' . y')


isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) = 
  (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))

isoList :: ISO a b -> ISO [a] [b]
isoList (x, y) = (map x, map y)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (ab, ba) = (liftM ab, liftM ba)

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (f, g)
  where
    f (Left a) = Left $ ab a
    f (Right c) = Right $ cd c
    g (Left b) = Left $ ba b
    g (Right d) = Right $ dc d

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) = (\f -> cd . f . ba, \f -> dc . f . ab)


isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b



isoUnMaybe (ab, ba) = (f, g)
  where
    f x = case ab (Just x) of
            Just y -> y
            Nothing -> case ab Nothing of
                            Nothing -> error "impossible"
                            Just y -> y
    g x = case ba (Just x) of
            Just y -> y
            Nothing -> case ba Nothing of
                            Nothing -> error "impossible"
                            Just y -> y

isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = (f, g)
  where
    f (Right ()) = Left []
    f (Left x) = Left $ () : x
    g (Left []) = Right ()
    g (Left (_:xs)) = Left xs

isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (f, g)
  where
    f (ab, ba) = (ba, ab)
    g (ab, ba) = (ba, ab)
    