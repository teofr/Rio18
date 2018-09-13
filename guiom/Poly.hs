{-# OPTIONS_GHC -Wall #-}
module Poly where
import Data.List (intercalate)

data Poly a = P [a]

asd = P [1,2,3,4,0,0,0]
bsd = P [1,2,3,4]
csd = P [1,2,4,4]

-- Exercise 1 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P a) (P b) = (dropWhile (\x -> x == 0) (reverse a)) == dropWhile (\x -> x == 0) (reverse b)
 
-- Exercise 2 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P a) = intercalate "+" $ reverse $ map combinar $ filter (\(a, _) -> a /= 0) (zip a $ ["","x"] ++ ["x^" ++ show y | y<-[2..]])
        where combinar (a, b) = show a ++ b

-- Exercise 3 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P p1) (P p2) = P $ sumita p1 p2
    where
        sumita [] y  = y
        sumita x []  = x
        sumita (x:xs) (y:ys) = x+y : sumita xs ys

-- Exercise 4 -----------------------------------------


times :: Num a => Poly a -> Poly a -> Poly a
times (P p1) (P p2) =  sum $ map P $ por p1 p2
    where
        por [] p2 = []
        por (p1:ps) p2 = (map (* p1) p2) : (map (0:) $ por ps p2)

-- Exercise 5 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger x = P [fromInteger x]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

