module Week42Exercise1 where

fromLeftAndRight :: (Either a b -> c) -> (a -> c, b -> c)
fromLeftAndRight f = (f . Left, f . Right)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left a) = f a
either' f g (Right b) = g b

toFstAndSnd :: (a -> (b,c)) -> (a -> b, a -> c)
toFstAndSnd f = (fst . f, snd . f)

pair :: (a -> b) -> (a -> c) -> a -> (b,c)
pair f g a = (f a, g a)