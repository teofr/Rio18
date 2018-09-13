module Functor where

newtype Aider a b = Aider (Either a b) deriving Show

instance Functor (Aider a) where

 fmap f (Aider (Right c)) = Aider $ Right $  f c
 fmap _ (Aider (Left c)) = (Aider (Left c)) -- :: Aider a c


data ComplicatedA a b
    = Con1 a b
    | Con2 [Maybe (a -> b)]

instance Functor (ComplicatedA a) where
    fmap f (Con1 a b) = Con1 a $ f b
    fmap f (Con2 xs) = Con2 $ fmap (fmap (f.)) xs

data ComplicatedB f g a b
    = Con3 (f a)
    | Con4 (g b)
    | Con5 (g (g [b]))

instance Functor g => Functor (ComplicatedB f g a) where
    fmap _ (Con3 x) = Con3 x
    fmap h (Con4 x) = Con4 $ h <$> x
    fmap h (Con5 x) = Con5 $ (\x -> (fmap h) <$> x) <$> x --fmap (fmap (fmap h)) x

