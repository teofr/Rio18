{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
-- Chequear si se puede sacar esto

data Zero
data Succ n

data True
data False

type Two = Succ (Succ (Zero))
type Three = Succ (Two)

class Even n b | n -> b where isEven :: n -> b
class Odd n b | n -> b where isOdd :: n -> b

instance Even Zero True
instance Odd n b => Even (Succ n) b
instance Odd Zero False
instance Even n b => Odd (Succ n) b

class Add a b c | a b -> c where add :: a -> b-> c
instance Add Zero b b
instance Add a b c => Add (Succ a) b (Succ c)

class Mul a b c | a b -> c where mul :: a -> b -> c
instance Mul Zero b Zero
instance (Mul a b c, Add b c d) => Mul (Succ a) b d

{-==========================[]=======================-}

f b n = if b then  paAbajo n  else isEven n  

paAbajo :: Succ n -> n
paAbajo x = undefined

paArriba :: n -> Succ n
paArriba x = undefined


{-
class Mul a b c | a b -> c where rant :: a->b->c

type Vec b = [b]
instance Mul a b c => Mul a (Vec b) (Vec c)

g b x y = if b then rant x [y] else y



-}
