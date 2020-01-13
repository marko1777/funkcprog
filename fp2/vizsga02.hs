module Vizsga02 where

import Control.Applicative
import Interpreter
import Syntax

data Tree a b = Leaf b | Node a (Tree a b) (Tree a b)
  deriving (Eq, Show)

ex1 :: Tree Int Int
ex1 = Node 1 (Leaf 10) (Node 2 (Leaf 20) (Leaf 30))

instance Functor (Tree a) where
    fmap f (Leaf v) = Leaf $ f v
    fmap f (Node v left right) = Node v (fmap f left) (fmap f right)

instance Foldable (Tree a) where
    foldr f b (Leaf v) = f v b 
    foldr f b (Node v left right) = foldr f (foldr f b right) left

instance Traversable (Tree a) where
    traverse f (Leaf v) = Leaf <$> f v
    traverse f (Node v left right) = Node v <$> (traverse f left) <*> (traverse f right)

bimap :: (a -> a') -> (b -> b') -> Tree a b -> Tree a' b'
bimap f g (Leaf v) = Leaf $ g v
bimap f g (Node v left right) = Node (f v) (bimap f g left) (bimap f g right)

bitraverse :: Applicative f => (a -> f a') -> (b -> f b') -> Tree a b -> f (Tree a' b')
bitraverse f g (Leaf v) = Leaf <$> g v
bitraverse f g (Node v left right) = Node <$> f v <*> (bitraverse f g left) <*> (bitraverse f g right)

annotateSumsImpl :: Tree Int a -> Int -> Tree Int Int
annotateSumsImpl (Node v left right) s = Node v (annotateSumsImpl left (s + v)) (annotateSumsImpl right (s + v)) 
annotateSumsImpl _ s = Leaf s 

annotateSums :: Tree Int a -> Tree Int Int
annotateSums t = annotateSumsImpl t 0

annotateSums' :: Tree a Int -> Tree Int Int
annotateSums' (Leaf v) = Leaf v 
annotateSums' (Node v left right) = Node (foldr (+) (foldr (+) 0 left) right) (annotateSums' left) (annotateSums' right)

numberElemsImpl :: Tree a b -> Int -> (Tree (a, Int) (b, Int), Int)
numberElemsImpl (Leaf v) n = (Leaf (v, n), n) 
numberElemsImpl (Node v left right) n = (Node (v, n) ll rr, mm) where
    (ll, m) = numberElemsImpl left (n + 1)
    (rr, mm) = numberElemsImpl right (m + 1)

numberElems :: Tree a b -> Tree (a, Int) (b, Int)
numberElems tree = fst $ numberElemsImpl tree 0

