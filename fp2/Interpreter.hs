module Interpreter where

import Control.Monad.State
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Maybe

import Syntax

data RTVal = RTLit Lit
  deriving (Eq, Ord, Show)

type Eval a = State (Map Var RTVal) a

evalLit :: Lit -> Eval RTVal
evalLit l = pure $ RTLit l

evalVar :: Var -> Eval RTVal
evalVar v = do
  varMap <- get
  case Map.lookup v varMap of
    Just rtVal -> pure rtVal
    Nothing -> error $ "Variable not found in environment: " ++ show v

foo :: Lit -> Maybe Bool
foo (LBool b) =Just b
foo _         = Nothing

bar :: Lit -> Maybe Int
bar (LInt b) = Just b
bar _        = Nothing

eval :: RTVal -> (Maybe Bool, Maybe Int)
eval (RTLit l) = case foo l of
  Just x -> (Just x, Nothing)
  _      -> (Nothing, bar l)

evalExpr :: Expr -> Eval RTVal
evalExpr (ELit l) = evalLit l
evalExpr (EVar v) = evalVar v
evalExpr (Plus e1 e2) = do
  r1 <- evalExpr e1 
  r2 <- evalExpr e2
  pure $ RTLit $ LInt $ fromJust(snd $ eval r1) + fromJust(snd $ eval r2)
  
evalExpr (Minus e1 e2) = do
  r1 <- evalExpr e1 
  r2 <- evalExpr e2
  pure $ RTLit $ LInt $ fromJust(snd $ eval r1) - fromJust(snd $ eval r2)
  
evalExpr (Mul e1 e2) = do
  r1 <- evalExpr e1 
  r2 <- evalExpr e2
  pure $ RTLit $ LInt $ fromJust(snd $ eval r1) * fromJust(snd $ eval r2)
  
evalExpr (And e1 e2) = do
  r1 <- evalExpr e1 
  r2 <- evalExpr e2
  pure $ RTLit $ LBool $ fromJust(fst $ eval r1) && fromJust(fst $ eval r2)
  
evalExpr (Eq e1 e2) = do
  r1 <- evalExpr e1 
  r2 <- evalExpr e2
  pure $ RTLit $ LBool $ 
    case eval r1 of 
      (Just x, Nothing) -> x == fromJust(fst $ eval r2)
      _ -> fromJust(snd $ eval r1) == fromJust(snd $ eval r2)
  
evalExpr (LEq e1 e2) = do
  r1 <- evalExpr e1 
  r2 <- evalExpr e2
  pure $ RTLit $ LBool $ fromJust(snd $ eval r1) <= fromJust(snd $ eval r2)
  
evalExpr (Not e) = do
  r <- evalExpr e
  pure $ RTLit $ LBool $ not $ fromJust(fst $ eval r)

evalWhile :: Statement -> Eval ()
evalWhile (Skip) = pure ()

evalWhile (Assign v e) = do
  varMap <- get
  rVal <- evalExpr e
  put $ Map.insert v rVal varMap

evalWhile (Seq s ss) = do
  evalWhile s
  evalWhile ss

evalWhile (If e s ss) = do
  rVal <- evalExpr e
  case rVal of
    (RTLit (LBool True)) -> evalWhile s
    (RTLit (LBool False)) -> evalWhile ss
    -- _ -> error
evalWhile (While e s) = do
  rVal <- evalExpr e
  case rVal of
    (RTLit (LBool True)) -> evalWhile((Seq s) (While e s))
    (RTLit (LBool False)) -> pure ()