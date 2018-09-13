{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

import Prelude hiding (tail, head, last, init, map, uncons)

data Nat = Z | S Nat

infixl 6 :+

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z     :+ m = m
type instance (S n) :+ m = S (n :+ m)


-- Ejercicio 1 --
-----------------

infixl 7 :*

type family (n :: Nat) :* (m :: Nat) :: Nat
type instance Z		:* m = Z
type instance (S n)	:* m = m :+ n :* m


-- GADTs --
-----------

data Vector a n where
  Nil  :: Vector a Z
  (:-) :: a -> Vector a n -> Vector a (S n)

infixr 5 :-

deriving instance Eq a   => Eq (Vector a n)
deriving instance Show a => Show (Vector a n)

head :: Vector a (S n) -> a
head (x :- _) = x

tail :: Vector a (S n) -> Vector a n
tail (_ :- xs) = xs

append :: Vector a n -> Vector a m -> Vector a (n :+ m)
append Nil		 ys = ys
append (x :- xs) ys = x :-  append xs ys

main :: IO ()
main = do
	print $ head l
	print $ tail l
	-- print $ head Nil -- Se rompe en compilacion
	where
		l = 1 :- 2 :- Nil

-- Ejercicio 2 --
-----------------

toList :: Vector a n -> [a]
toList Nil = []
toList (x :- xs) = x : toList xs

data SNat n where
	ZNat :: SNat Z
	SS :: SNat n -> SNat (S n)

fromList ::  SNat n -> [a] -> Vector a n
fromList ZNat [] = Nil
fromList (SS n) (x:xs) = x :- fromList n xs

map :: (a -> b) -> Vector a n -> Vector b n
map _ Nil = Nil
map f (x:-xs) = f x :- map f xs

uncons :: Vector a (S n) -> Maybe (a, Vector a n)
uncons (x :- xs) = Just (x, xs)

init :: Vector a (S n) -> Vector a n
init (x :- Nil) = Nil
init (x :- y :- xs) = x :- init (y :- xs)

last :: Vector a (S n) -> a
last (x :- Nil) = x
last (x :- y :- xs) = last (y :- xs)

zipWithSame :: (a -> b -> c) -> Vector a n -> Vector b n -> Vector c n
zipWithSame _ Nil Nil = Nil
zipWithSame f (x :- xs) (y :- ys) = f x y :- zipWithSame f xs ys

type family Min (n :: Nat) (m :: Nat) :: Nat
type instance Min Z m = Z
type instance Min n Z = Z
type instance Min (S n) (S m) = S (Min n m)
