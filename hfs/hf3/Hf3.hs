module Hf3 where

isUnderscore :: Char -> Bool

isUnderscore a = (a == '_') 

isDigit :: Char -> Bool

isDigit a = elem a ['0'..'9']


isLetter :: Char -> Bool

isLetter a = elem a ['a'..'z'] || elem a ['A'..'Z']

isValid :: Char -> Bool

isValid a = isLetter(a) || isDigit(a) || isUnderscore(a)

isValidUsername :: String -> Bool

isValidUsername a = not(elem False (map isValid a))

