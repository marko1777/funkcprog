module Hf5 where

import Prelude hiding (length)

length :: Num b => [a] -> b

length [] = 0
length (x:xs) = 1 + length xs

remove :: Eq a => a -> [a] -> [a]

remove a (x:xs)
    | (a == x) = xs
    | otherwise = x : remove a xs

remove a [] = []
