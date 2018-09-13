{-#LANGUAGE MultiParamTypeClasses#-}
{-#LANGUAGE FlexibleInstances#-}

import qualified Data.Set as S




class Eq e => Collection c e where
    insert :: c -> e -> c
    member :: c -> e -> Bool

instance Eq a => Collection [a] a where
    insert xs x = x:xs
    member = flip elem

instance (Ord a, Eq a) => Collection (S.Set a) a where
	insert = flip S.insert
	member = flip S.member


esFalso = member ( insert (S.empty :: S.Set Int) (1 :: Int) ) ( 2:: Int)
