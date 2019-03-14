module Hf4 where

import Data.Char as Char (toUpper)

toUpperFirst :: String -> String

toUpperFirst [] = []
toUpperFirst (x:xs) = toUpper x : xs

toUpperFirsts :: String -> String

toUpperFirsts s = unwords(map toUpperFirst (words s))