-- Si GHC se queja y hable de FlexibleInstances, vean
-- en https://prime.haskell.org/wiki/FlexibleInstances


{-# LANGUAGE FlexibleInstances #-}

module Overlap where

class ToInt a where
  f :: a -> Int

instance ToInt Int where
  f i = i

-- todos los caracteres se convierten a 1
instance ToInt Char where
  f _ = 1

-- convertir Integer a Int usando fromIntegral
instance ToInt Integer where
  f i = fromIntegral i

-- Listas.

-- En el caso general, convertir los elementos
-- de la lista a Int y luego sumarlos
-- > f ([1,2,3] :: [Integer])
-- 6
-- > f "Abracadabra"
-- 11
instance ToInt a => ToInt [a] where
  f l = sum (map f l)

-- En el caso particular de lista de Int, sumar
-- directamente los elementos de la lista y luego
-- sumarle 10 
-- > f ([1,2,3] :: [Int])
-- > 16
instance {-# OVERLAPPING  #-} ToInt [Int] where
  f l = sum l + 1

