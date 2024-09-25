module Utils (pairs, takeWhile_) where

pairs :: [b] -> [(b, b)]
pairs [] = []
pairs [_] = []
pairs (x : y : xs) = (x, y) : pairs xs

takeWhile_ :: (a -> Bool) -> [a] -> [a]
takeWhile_ p = foldr (\x ys -> if p x then x : ys else [x]) []
