{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
-- Chequear si se puede sacar esto

data Nil 
data Cons x xs 
type Singleton a = Cons a Nil
-- por que no pueden ser tipos vacios?

data False
data True

data Zero
data Succ n

class DownFrom n xs | n -> xs where downFrom :: n -> xs
instance DownFrom Zero Nil
instance DownFrom n xs => DownFrom (Succ n) (Cons n xs)

class Lte a b c | a b -> c where lte :: a -> b -> c
-- a <= b
instance Lte Zero b True
instance Lte (Succ n) Zero False
instance Lte a b c => Lte (Succ a) (Succ b) c

class Insert x xs ys | x xs -> ys where insert :: x -> xs -> ys
-- inserta x ordenado en xs, retorna ys
instance Insert x Nil (Singleton x)
instance (Lte x y b, InsertCons b x y ys r) => Insert x (Cons y ys) r
-- Por ahi se puede hacer una clase IFTE

class InsertCons b x1 x2 xs ys | b x1 x2 xs -> ys
instance InsertCons True x1 x2 xs (Cons x1 (Cons x2 xs))
instance Insert x1 xs ys => InsertCons False x1 x2 xs (Cons x2 ys)

class Sort xs ys | xs -> ys where sort :: xs -> ys
instance Sort Nil Nil
instance (Sort xs ys, Insert x ys zs) => Sort (Cons x xs) zs


{-=============================-}

class Ifte b t e r | b t e -> r where ifte :: b -> t -> e -> r
instance Ifte True t e t
instance Ifte False t e e

class Insert' x xs ys | x xs -> ys where insert' :: x -> xs -> ys
instance Insert' x Nil (Singleton x)
instance (Lte x y b, Insert' x ys r', Ifte b (Cons x (Cons y ys)) (Cons y r') r) => Insert' x (Cons y ys) r

class Sort' xs r | xs -> r where sort' :: xs -> r
instance Sort' Nil Nil
instance (Sort' xs r', Insert' x r' r) => Sort' (Cons x xs) r

{-=============================-}

type N1 = Succ Zero
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3
type N5 = Succ N4
type N6 = Succ N5
type N7 = Succ N6
type N8 = Succ N7
type N9 = Succ N8
type N10 = Succ N9