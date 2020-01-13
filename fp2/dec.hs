module Solution where

import Data.Map (Map(..))
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Error.Class

------------------ EXERCISES -----------------

import Data.Semigroup
import Data.List

data RoseTree a
    = Branch a [RoseTree a]
    deriving (Eq, Ord, Show)

ex1 :: RoseTree Int
ex1 = Branch 2 $
        [ Branch 3 $
            [ Branch 11 []
            ]
        , Branch 5 $ []
        , Branch 7 $
            [ Branch 13 []
            ]
        ]

instance Functor RoseTree where
    fmap f (Branch a xs) = Branch (f a) $ map (fmap f) xs

instance Foldable RoseTree where
    foldMap f (Branch a xs) = foldr (<>) (f a) (map (foldMap f) xs)

countElems :: RoseTree a -> Int
countElems rt = foldr (\ _ n -> n + 1) 0 rt

maxElem :: Ord a => RoseTree a -> a
maxElem (Branch a xs) = foldr (\x y -> if x < y then y else x) a (map maxElem xs)

instance Traversable RoseTree where
    traverse f (Branch a xs) = Branch <$> f a <*> traverse (traverse f) xs

numberElems :: RoseTree a -> RoseTree (a, Int)
numberElems rt = undefined

safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:xs) 0 = Just x
safeIndex (_:xs) n = safeIndex xs $ n - 1

transformWithList :: [a] -> RoseTree Int -> Maybe (RoseTree a)
transformWithList list (Branch a xs) = do
    val <- safeIndex list a
    branches <- traverse (transformWithList list) xs
    Just $ Branch val branches


-------------------- SYNTAX --------------------

data Lit
  = LBool Bool
  | LInt Int
  | LStr String
  deriving (Eq, Ord, Show)

type Name = String

newtype Var = Var Name
  deriving (Eq, Ord, Show)

data Expr
  -- atoms
  = ELit Lit
  | EVar Var
  -- arithmetic
  | Plus Expr Expr
  | Minus Expr Expr
  | Mul Expr Expr
  -- logical
  | And Expr Expr
  | Eq Expr Expr
  | LEq Expr Expr
  | Not Expr
  -- concat
  | Concat Expr Expr
  deriving (Eq, Ord, Show)

data Statement
  = Skip
  | Seq Statement Statement
  | If Expr Statement Statement
  | While Expr Statement
  | Assign Var Expr
  deriving (Eq, Ord, Show)



-------------------- PARSER BASE --------------------

type Parser a = StateT String Maybe a

runParser :: Parser a -> String -> Maybe (a, String)
runParser = runStateT

evalParser :: Parser a -> String -> Maybe a
evalParser p s = fmap fst (runParser p s)

eof :: Parser ()
eof = do
  str <- get
  case str of
    "" -> do
      put ""
      pure () -- lift $ Just (), lift $ pure ()
    _  -> lift $ Nothing

char :: Char -> Parser Char
char c = do
  str <- get
  case str of
    (x:xs) | x == c -> do
      put xs
      pure c
    _ -> lift $ Nothing


lowerAlpha :: Parser Char
lowerAlpha = foldr (<|>) empty $ map char ['a'..'z']

capitalAlpha :: Parser Char
capitalAlpha = foldr (<|>) empty $ map char ['A'..'Z']

digitChar :: Parser Char
digitChar = foldl (<|>) empty $ map char ['0'..'9']

digit :: Parser Int
digit = fmap (\n -> n - 48)
      . fmap fromEnum
      . foldl (<|>) empty
      $ map char ['0'..'9']

natural :: Parser Int
natural = foldl1 (\acc cur -> 10*acc + cur) <$> some digit

token' :: String -> Parser ()
token' str = void $ traverse char str

token :: String -> Parser ()
token str = token' str <* ws

ws :: Parser ()
ws = void $ many (char ' ')



-------------------- PARSER WHILE --------------------

iLit :: Parser Lit
iLit = (LInt <$> natural) <* ws

bLit :: Parser Lit
bLit = token "true"  *> pure (LBool True)
    <|> token "false" *> pure (LBool False)

sLit :: Parser Lit
sLit = fmap LStr $ char '\"' *> many (lowerAlpha <|> capitalAlpha <|> digitChar <|> char ' ') <* char '\"'

lit :: Parser Lit
lit = iLit <|> bLit <|> sLit

parens :: Parser a -> Parser a
parens p = token "(" *> p <* token ")"

-- "3+5" -> EPlus (ELit (Lit 3)) (ELit (Lit 5))
expr' :: Parser Expr
expr' = ELit <$> lit
    <|> parens expr

expr :: Parser Expr
expr = Plus <$> expr' <*> (token "+"  *> expr)
    <|> And  <$> expr' <*> (token "&&" *> expr)
    <|> Concat <$> expr' <*> (token "++" *> expr)
    <|> expr'

var :: Parser Var
var = (Var <$> some lowerAlpha) <* ws

statement' :: Parser Statement
statement' = (token "Skip" *> pure Skip)
        <|> Assign <$> var <*> (token ":=" *> expr)
        <|> If <$> (token "If"   *> expr)
                <*> (token "then" *> statement)
                <*> (token "else" *> statement)
        <|> While <$> (token "While" *> expr)
                  <*> (token "do"    *> statement <* token "end")

statement :: Parser Statement
statement = Seq <$> statement' <*> (token ";" *> statement)
        <|> statement'
  


-------------------- INTERPRETER --------------------


data RTVal = RTLit Lit
  deriving (Eq, Ord, Show)

type VarMapping = Map Var RTVal

type Eval a = StateT VarMapping (ExceptT String Identity) a

runEval :: Eval a -> VarMapping -> Either String (a, VarMapping)
runEval m s = runExcept (runStateT m s)

evalEval :: Eval a -> VarMapping -> Either String a
evalEval m s = fst <$> runEval m s

evalLit :: Lit -> Eval RTVal
evalLit lit = return $ RTLit lit

evalVar :: Var -> Eval RTVal
evalVar v = do
  vars <- get
  let mVal = Map.lookup v vars
  case mVal of
    Just val -> return val
    Nothing  -> throwError $ "Undefined variable: " ++ show v

evalBinOp :: (Expr -> Eval a) ->
             (Expr -> Eval b) ->
             (c -> RTVal) ->
             (a -> b -> c) ->
             (Expr -> Expr -> Eval RTVal)
evalBinOp evalLhs evalRhs mkRetVal op lhs rhs = do
  lhs' <- evalLhs lhs
  rhs' <- evalRhs rhs
  let result = lhs' `op` rhs'
  return $ mkRetVal result

evalInt :: Expr -> Eval Int
evalInt e = do
  e' <- evalExpr e
  case e' of
    RTLit (LInt n) -> return n
    _ -> throwError $ show e ++ " does not evaluate to an Integer"

evalStr :: Expr -> Eval String
evalStr e = do
  e' <- catchError (evalExpr e) (\e -> if isPrefixOf "Undefined variable: " e then pure (RTLit $ LStr "") else throwError e)
  case e' of
    RTLit (LStr n) -> return n
    _ -> throwError $ show e ++ " does not evaluate to an Integer"

evalBool :: Expr -> Eval Bool
evalBool e = do
  e' <- evalExpr e
  case e' of
    RTLit (LBool b) -> return b
    _ -> throwError $ show e ++ " does not evaluate to a Boolean"

mkRTInt :: Int -> RTVal
mkRTInt = RTLit . LInt

mkRTBool :: Bool -> RTVal
mkRTBool = RTLit . LBool

mkRTStr :: String -> RTVal
mkRTStr = RTLit . LStr

evalUnaryOp :: (Expr -> Eval a) ->
               (b -> RTVal) ->
               (a -> b) ->
               (Expr -> Eval RTVal)
evalUnaryOp evalArg mkRetVal op arg =
  evalBinOp evalArg evalArg mkRetVal (const <$> op) arg arg
  -- const <$> op is similar to: \lhs rhs -> op lhs

evalExpr :: Expr -> Eval RTVal
evalExpr (ELit l) = evalLit l
evalExpr (EVar v) = evalVar v
evalExpr (Plus lhs rhs) = evalBinOp evalInt evalInt mkRTInt (+) lhs rhs
evalExpr (Minus lhs rhs) = evalBinOp evalInt evalInt mkRTInt (-) lhs rhs
evalExpr (Mul lhs rhs) = evalBinOp evalInt evalInt mkRTInt (*) lhs rhs
evalExpr (And lhs rhs) = evalBinOp evalBool evalBool mkRTBool (&&) lhs rhs
evalExpr (LEq lhs rhs) = evalBinOp evalInt evalInt mkRTBool (<=) lhs rhs
evalExpr (Not arg) = evalUnaryOp evalBool mkRTBool (not) arg
evalExpr (Concat lhs rhs) = evalBinOp evalStr evalStr mkRTStr (++) lhs rhs

