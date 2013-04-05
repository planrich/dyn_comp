module Interpretor 
    ( eval
    , SymbolTable (..)
    , harvestSymbols
    , mainExpr
    , findSymsNameStartsWith
    , Env
    )
  where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Error
import Data.Maybe

import ParserTypes
import Environment
import Builtins

-- |Evaluate an expression in a certain environment.
eval :: Env -> Expr -> ThrowError Expr
eval env (AppExpr f s) = apply env f s
eval env (ListExpr es) = liftM ListExpr $ mapM (eval env) es
eval env (VarExpr n) = do
    mEntry <- return $ findEntry env n
    case mEntry of
        (Just (SymBinding expr)) -> eval env expr
        Nothing -> throwError $ SymbolNotFound (n)
-- ^ A conditional expression
eval env (CondExpr fact alt1 alt2) = do
    case holds env fact of
        Left error -> throwError error
        Right True -> eval env alt1
        Right False -> eval env alt2
eval env e
    | isAtom e = return $ e -- cannot reduce further
    | otherwise =  return $ e

apply :: Env -> Expr -> Expr -> ThrowError Expr
-- ^ Nested application recursion
apply env a@(AppExpr _ _) args = (eval env a) >>= (\a -> apply env a args)
-- ^Apply a function context to another arg. Basically there are two options:
-- The arguments suffice the function -> function is executed
-- There are too little arguemts -> context swallows argument and proceeds
apply env c@(FuncCtx f args) arg
    | (length args) + 1 == builtinParamCount f = (eval env arg) >>= (\arg -> (builtinFunction f) (args ++ [arg]))
    | otherwise = (eval env arg) >>= (\arg -> return $ FuncCtx f (args ++ [arg]))
-- ^At the bottom of a recursion an AppExpr might contain a Builtin or call any other
-- function that is defined within the environment.
apply env (VarExpr n) args =
    case M.lookup n builtins of
        -- if it is a unary function handle it right away
        Just (Builtin 1 f) -> (eval env args) >>= (\args -> f [args])
        -- n-ary function must harvest additional n-1 parameters
        Just b -> (eval env args) >>= (\args -> return $ FuncCtx b [args])
        Nothing -> applyFromEnv env n args
apply env (LamExpr n e) arg = (subs e n arg) >>= eval env
-- ^This is most likely an error.
apply env func args = throwError $ CannotApply func args

applyFromEnv :: Env -> Name -> Expr -> ThrowError Expr
applyFromEnv env name arg = do
    mFunc <- return $ findFunc env name
    case mFunc of
        Nothing -> throwError $ SymbolNotFound name
        Just func -> do
            case funcArgCount func of
                0 -> evalPattern env [] (funcPatterns func) >>= (\expr -> apply env expr arg)
                1 -> evalPattern env [arg] (funcPatterns func)
                --n -> FuncCtx 
              {-
    case argCount of
      0 -> do
        pat <- return ((match [] (funcPatterns func))
        eval env expr
    if argCount == 1
      then do
        case pat of
            Just ps@(Pattern binding expr) -> eval env (wrapLambdas binding expr)
            _ -> case sym of
                    Nothing -> throwError $ SymbolNotFound n
                    _ -> throwError $ PatternFallthrough n [arg]
      else
      -}

evalPattern :: Env -> [Expr] -> [Pattern] -> ThrowError Expr
evalPattern env args (p:ps) = undefined

wrapLambdas :: [Binding] -> [Expr] -> Expr -> Expr
wrapLambdas (b:bs) (e:es) ex = undefined

-- |Subsitute a variable in an expression with another expression
subs :: Expr -> Name -> Expr -> ThrowError Expr
subs (AppExpr e1 e2) old new = (subs e1 old new) >>= (\e1 -> (subs e2 old new) >>= (\e2 -> return $ AppExpr e1 e2))
subs (ListExpr es) old new = liftM ListExpr (mapM (\e -> subs e old new) es)
subs (LamExpr n e) old new = liftM (LamExpr n) (subs e old new)
subs v@(VarExpr x) old new
    | old == x = return new -- replace because they really match
    | otherwise = return v
subs old _ _ = return old

-- |Find out whether an expr is a variable bool literal string or list
isAtom :: Expr -> Bool
isAtom (VarExpr _) = True
isAtom (BoolExpr _) = True
isAtom (LitExpr _) = True
isAtom (ListExpr _) = True
isAtom (StrExpr _) = True
isAtom _ = False

-- |Does an evaluated expression equal ture?
holds :: Env -> Expr -> ThrowError Bool
holds env e = do
    case eval env e of
        Right (BoolExpr True) -> return $ True
        Right (BoolExpr False) -> return $ False
        Right e -> throwError $ ConditionalNotABool e
        Left error -> throwError $ error

