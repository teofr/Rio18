{-# LANGUAGE RankNTypes, GADTSyntax, GADTs, TypeFamilies #-}



type family M a where
	M a = Int

f :: M a -> b
f _ = undefined