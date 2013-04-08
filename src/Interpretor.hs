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
apply env c@(FuncCtx bi@(Builtin params f)  args) arg
    | (length args) + 1 == params = (eval env arg) >>= (\arg -> f (args ++ [arg]))
    | otherwise = (eval env arg) >>= (\arg -> return $ FuncCtx bi (args ++ [arg]))
-- ^Of course there is a defined function
apply env c@(FuncCtx de@(Defined params f)  args) arg
    | (length args) + 1 == params = (eval env arg) >>= (\arg -> evalPattern (funcName f) env (args ++ [arg]) (funcPatterns f))
    | otherwise = (eval env arg) >>= (\arg -> return $ FuncCtx de (args ++ [arg]))
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

--execFunction :: Env -> Func -> Expr -> ThrowError Expr
--execFunction env func expr' =
applyFromEnv :: Env -> Name -> Expr -> ThrowError Expr
applyFromEnv env name arg = do
    mFunc <- return $ findFunc env name
    case mFunc of
        Nothing -> throwError $ SymbolNotFound name
        Just func -> do
            case funcArgCount func of
                0 -> evalPattern name env [] (funcPatterns func) >>= (\expr -> apply env expr arg)
                1 -> evalPattern name env [arg] (funcPatterns func)
                n -> return $ FuncCtx (Defined n func) [arg]

evalPattern :: Name -> Env -> [Expr] -> [Pattern] -> ThrowError Expr
evalPattern fname _ es [] = throwError $ PatternFallthrough fname es
evalPattern fname env es (p:ps) = do
    case matchPattern p es of
        Just _ -> eval env (wrapLambdas fname (patternBindings p) es (patternExpr p))
        --Just _ -> throwError $ Fallback $ show (wrapLambdas fname (patternBindings p) es (patternExpr p)) ++ (show fname) ++(show es) ++ (show (patternBindings p)) ++ (show (patternExpr p))
        Nothing -> trynext
  where
    trynext = evalPattern fname env es ps

wrapLambdas :: Name -> [Binding] -> [Expr] -> Expr -> Expr
wrapLambdas fname [] [] ex = ex
wrapLambdas fname (b@(BVar name):bs) (e:es) ex = (AppExpr (wrapLambdas fname bs es (LamExpr name ex)) e)
wrapLambdas fname (_:bs) (e:es) ex = wrapLambdas fname bs es ex

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

