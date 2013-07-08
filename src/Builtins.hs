--
-- Builtins.hs
-- Copyright (C) 2013 rich <planrichi@gmail.com>
--

module Builtins 
    ( builtins
    , existsBuiltin
    , findBuiltin
    )
  where

import qualified Data.Map as M
import Control.Monad.Error
import Data.Maybe
import qualified Data.List as L

import ParserTypes


builtins :: M.Map String Builtin
builtins = M.fromList [ ("add", Builtin 2 $ numericBinary (+))
                      , ("sub", Builtin 2 $ numericBinary (-))
                      , ("div", Builtin 2 $ numericBinary div)
                      , ("mul", Builtin 2 $ numericBinary (*))
                      , ("mod", Builtin 2 $ numericBinary mod)
                      , ("and", Builtin 2 $ boolBinary (&&))
                      , ("or",  Builtin 2 $ boolBinary (||))

                      , ("equal",  Builtin 2 $ comparison (==))
                      , ("nqual",  Builtin 2 $ comparison (/=))
                      , ("less", Builtin 2 $ comparison (<))
                      , ("less_equal", Builtin 2 $ comparison (<=))
                      , ("greater", Builtin 2 $ comparison (>))
                      , ("greater_equal", Builtin 2 $ comparison (>=))
                      , ("ltos", Builtin 1 ltos)

                      , ("cons", Builtin 2 $ cons)
                      , ("append", Builtin 2 $ append)

                      , ("undefined", Builtin 0 undef)
                      , ("fatal", Builtin 1 fatal)

                      , ("at", Builtin 2 at)

                      , ("sort", Builtin 1 sortList)
                      ]

ltos :: [Expr] -> ThrowError Expr
ltos [(ListExpr list)] = charListToStr list ""

charListToStr :: [Expr] -> String -> ThrowError Expr
charListToStr ((CharExpr c):cs) str = charListToStr cs (str ++ [c])
charListToStr [] str = return $ StrExpr str
charListToStr list str = throwError $ Fallback $ "couldn't convert list: " ++ (show list)


sortList :: [Expr] -> ThrowError Expr
sortList [(ListExpr list)] = return $ ListExpr (L.sort list)
sortList e = throwError $ Fallback ("cannot sort: " ++ (show e))

at :: [Expr] -> ThrowError Expr
at [(ListExpr list), (LitExpr i)] = return $ list !! (fromIntegral i)
at _ = throwError $ Fallback "at could not be used on the params"


fatal :: [Expr] -> ThrowError Expr
fatal [e] = throwError $ Fallback ("fatal occured: " ++ (show e))
fatal _ = throwError $ Fallback "raised fatal with more than one param"

undef :: [Expr] -> ThrowError Expr
undef _ = throwError $ Fallback "hit undefined! exiting!"

append :: [Expr] -> ThrowError Expr
append [(ListExpr ls), a] = return $ ListExpr (ls ++ [a])
append [(StrExpr str), (CharExpr c)] = return $ StrExpr (str ++ [c])
append [a,b] = throwError $ Fallback ("could not append " ++ (show b) ++ " to " ++ (show a))

cons :: [Expr] -> ThrowError Expr
--cons [(CharExpr c), (ListExpr [])] = return $ StrExpr (c:[])
cons [a, (ListExpr l)] = return $ ListExpr (a:l)
cons [(CharExpr c), (StrExpr l)] = return $ StrExpr (c:l)
cons [a,b] = throwError $ Fallback ("could not prepend " ++ (show a) ++ " to " ++ (show b))

existsBuiltin :: Int -> String -> Maybe Builtin
existsBuiltin argcount name =
    case M.lookup name builtins of
        Just bi@(Builtin argcount' b) -> if argcount' == argcount then Just bi else Nothing
        _ -> Nothing

findBuiltin :: Name -> Maybe Builtin
findBuiltin name = M.lookup name builtins

comparison :: (Expr -> Expr -> Bool) -> [Expr] -> ThrowError Expr
comparison f [e1, e2]
    | e1 `f` e2 = return $ BoolExpr True
    | otherwise = return $ BoolExpr False


extractListOp :: Int -> ([Expr] -> Expr) -> [Expr] -> ThrowError Expr
extractListOp c f [(ListExpr ex)] = if length ex >= c then return $ f ex else throwError $ InvalidArgument "provide more arguments for that operation"
extractListOp _ _ (ex:_) = throwError $ TypeMissmatch "expected list" ex

boolBinary :: (Bool -> Bool -> Bool) -> [Expr] -> ThrowError Expr
boolBinary op [(BoolExpr a1), (BoolExpr a2)] = return $ BoolExpr $ op a1 a2
--boolBinary op [e1@(BoolExpr a1), e2] = (eval e2) >>= (\e2 -> boolBinary op [e1,e2])
--boolBinary op [e2,e1@(BoolExpr a1)] = (eval e1) >>= (\e1 -> boolBinary op [e1,e2])
boolBinary op (e1:_) = throwError $ TypeMissmatch "expected bool" e1 

numericBinary :: (Int -> Int -> Int) -> [Expr] -> ThrowError Expr
numericBinary op [(LitExpr a1), (LitExpr a2)] = return $ LitExpr $ op a1 a2
--numericBinary op [e1@(LitExpr a1), e2@(AppExpr _ _)] = (eval e2) >>= (\e2 -> numericBinary op [e1, e2])
--numericBinary op [e1@(AppExpr _ _), e2@(LitExpr a1)] = (eval e1) >>= (\e1 -> numericBinary op [e1, e2])
numericBinary op (e1:_) = throwError $ TypeMissmatch "expected number" e1 

