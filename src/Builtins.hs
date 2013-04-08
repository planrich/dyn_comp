--
-- Builtins.hs
-- Copyright (C) 2013 rich <planrichi@gmail.com>
--

module Builtins 
    ( builtins
    , existsBuiltin
    )
  where

import qualified Data.Map as M
import Control.Monad.Error
import Data.Maybe

import ParserTypes

builtins :: M.Map String Builtin
builtins = M.fromList [ ("add", Builtin 2 $ numericBinary (+))
                      , ("+",   Builtin 2 $ numericBinary (+))
                      , ("sub", Builtin 2 $ numericBinary (-))
                      , ("-",   Builtin 2 $ numericBinary (-))
                      , ("div", Builtin 2 $ numericBinary div)
                      , ("/",   Builtin 2 $ numericBinary div)
                      , ("mul", Builtin 2 $ numericBinary (*))
                      , ("*",   Builtin 2 $ numericBinary (*))
                      , ("mod", Builtin 2 $ numericBinary mod)
                      , ("and", Builtin 2 $ boolBinary (&&))
                      , ("or",  Builtin 2 $ boolBinary (||))
                      --, ("==",  Builtin 2 $ comparison (==))
                      --, ("!=",  Builtin 2 $ comparison (/=))
                      --, ("head", Builtin 1 $ extractListOp 1 (head))
                      --, ("tail", Builtin 1 extractListOp (tail))
                      ]

existsBuiltin :: Int -> String -> Maybe Builtin
existsBuiltin argcount name =
    case M.lookup name builtins of
        Just bi@(Builtin argcount' b) -> if argcount' == argcount then Just bi else Nothing
        _ -> Nothing

{-
comparison :: Eq a => (a -> a -> Bool) -> [Expr] -> ThrowError Expr
comparison f [(VarExpr x),(LitExpr y)]
    | f x y = return (BoolExpr True)
    | otherwise = return (BoolExpr False)
comparison f [(StrExpr x),(StrExpr y)]
    | f x y = return $ BoolExpr True
    | otherwise = return $ BoolExpr False
-- TODO add alot more
comparison _ _ = return $ BoolExpr False
-}


extractListOp :: Int -> ([Expr] -> Expr) -> [Expr] -> ThrowError Expr
extractListOp c f [(ListExpr ex)] = if length ex >= c then return $ f ex else throwError $ InvalidArgument "provide more arguments for that operation"
extractListOp _ _ (ex:_) = throwError $ TypeMissmatch "expected list" ex

boolBinary :: (Bool -> Bool -> Bool) -> [Expr] -> ThrowError Expr
boolBinary op [(BoolExpr a1), (BoolExpr a2)] = return $ BoolExpr $ op a1 a2
--boolBinary op [e1@(BoolExpr a1), e2] = (eval e2) >>= (\e2 -> boolBinary op [e1,e2])
--boolBinary op [e2,e1@(BoolExpr a1)] = (eval e1) >>= (\e1 -> boolBinary op [e1,e2])
boolBinary op (e1:_) = throwError $ TypeMissmatch "expected bool" e1 

numericBinary :: (Integer -> Integer -> Integer) -> [Expr] -> ThrowError Expr
numericBinary op [(LitExpr a1), (LitExpr a2)] = return $ LitExpr $ op a1 a2
--numericBinary op [e1@(LitExpr a1), e2@(AppExpr _ _)] = (eval e2) >>= (\e2 -> numericBinary op [e1, e2])
--numericBinary op [e1@(AppExpr _ _), e2@(LitExpr a1)] = (eval e1) >>= (\e1 -> numericBinary op [e1, e2])
numericBinary op (e1:_) = throwError $ TypeMissmatch "expected number" e1 

