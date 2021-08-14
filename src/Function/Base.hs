module Function.Base () where

import Prelude hiding (flip)

f :: Int -> Int -> Int
f x y = x * 10 + y

-- >>> f 3 5
-- 35

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

f' = flip f

-- >>> f' 3 5
-- 53

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

fa = (* 10)
fb = (+ 1)

-- >>> (fa `compose` fb) 7
-- 80
