{-# LANGUAGE BangPatterns #-}
module Interpretor 
    ( eval
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
eval env (AppExpr f s) = --apply env f s
    do s' <- eval env s
       apply env f s'
--eval env le@(ListExpr es) = return le --liftM ListExpr $ mapM (eval env) es
eval env (VarExpr n) = do
    mEntry <- return $ findEntry env n
    case mEntry of
        (Just (SymBinding expr)) -> eval env expr
        (Just (SymFunc func)) -> return $ FuncCtx (Defined (funcArgCount func) func) []
        Nothing -> do
            case M.lookup n builtins of
                Just b -> return $ FuncCtx b []
                Nothing -> throwError $ SymbolNotFound (n)

-- ^ A conditional expression
eval env (CondExpr fact alt1 alt2) = do
    case holds env fact of
        Left error -> throwError error
        Right True -> eval env alt1
        Right False -> eval env alt2
eval env (LetExpr name definition expr) = do
    def <- eval env definition
    computed <- (subs expr name def)
    eval env computed
eval env e
    | isAtom e = return $ e -- cannot reduce further
    | otherwise =  return $ e

apply :: Env -> Expr -> Expr -> ThrowError Expr
-- ^ Nested application recursion
apply env a@(AppExpr _ _) args = do
    a' <- (eval env a)
    apply env a' args
-- ^Apply a function context to another arg. Basically there are two options:
-- The arguments suffice the function -> function is executed
-- There are too little arguemts -> context swallows argument and proceeds
apply env c@(FuncCtx bi@(Builtin params f)  args) arg
    | (length args) + 1 == params = do
        --arg' <- (eval env arg)
        f (args ++ [arg])
    | otherwise = do
        --arg' <- (eval env arg)
        return $ FuncCtx bi (args ++ [arg])
-- ^Of course there is a defined function
apply env c@(FuncCtx de@(Defined params f)  args) arg
    | (length args) + 1 == params = do
        --arg' <- (eval env arg)
        matchAndEvalPatternExpr (functionName f) env (args ++ [arg]) (functionPatterns f)
    | otherwise = do
        -- arg' <- (eval env arg)
        return $ FuncCtx de (args ++ [arg])
-- ^At the bottom of a recursion an AppExpr might contain a Builtin or call any other
-- function that is defined within the environment.
apply env (VarExpr n) args =
    case M.lookup n builtins of
        -- if it is a unary function handle it right away
        Just (Builtin 1 f) -> do
            --args <- (eval env args)
            f [args]
        -- n-ary function must harvest additional n-1 parameters
        Just b -> do
            --args <- (eval env args)
            return $ FuncCtx b [args]
        Nothing -> do
            --args <- (eval env args) 
            applyFromEnv env n args
apply env (LamExpr n e) arg = do
    arg' <- subs e n arg
    eval env arg'
-- ^This is most likely an error.
apply env func args = throwError $ CannotApply func args

applyFromEnv :: Env -> Name -> Expr -> ThrowError Expr
applyFromEnv env name arg = do
    case findFunc env name of
        Nothing -> throwError $ SymbolNotFound name
        Just func -> do
            case funcArgCount func of
                0 -> matchAndEvalPatternExpr name env [] (functionPatterns func) >>= (\expr -> apply env expr arg)
                1 -> matchAndEvalPatternExpr name env [arg] (functionPatterns func)
                n -> return $ FuncCtx (Defined n func) [arg]

-- |Evaluate a pattern if it matches the given parameters.
matchAndEvalPatternExpr :: Name -> Env -> [Expr] -> [Pattern] -> ThrowError Expr
matchAndEvalPatternExpr fname _ es [] = throwError $ PatternFallthrough fname es
matchAndEvalPatternExpr fname env es (p:ps) = do
    case matchPattern p es of
        Just _ -> do
            wrapped <- subsBinding fname (patternBindings p) es (patternExpr p) 
            eval env wrapped
        Nothing -> matchAndEvalPatternExpr fname env es ps

subsBinding :: Name -> [Binding] -> [Expr] -> Expr -> ThrowError Expr
subsBinding fname [] [] ex = return $ ex
subsBinding fname (b@(BVar name):bs) (e:es) ex = do
    subs' <- (subs ex name e)
    subsBinding fname bs es subs'
subsBinding fname ((BList ((BVar head'),(BVar tail'))):bs) es'@(e:es) ex
    | isListExpr e = do
        sub1 <- (subs ex head' (listHead e))
        sub2 <- (subs sub1 tail' (listTail e))
        subsBinding fname bs es sub2
    | otherwise = throwError $ Fallback $ "could not match " ++ (show e) ++ " agains a list or string"
subsBinding fname ((BList ((BVar head'),_)):bs) es'@(e:es) ex
    | isListExpr e = do
        subs' <- (subs ex head' (listHead e))
        subsBinding fname bs es subs'
    | otherwise = throwError $ Fallback $ "could not match " ++ (show e) ++ " agains a list or string"
subsBinding fname ((BList (_,(BVar tail'))):bs) es'@(e:es) ex
    | isListExpr e = do 
        subs' <- (subs ex tail' (listTail e))
        subsBinding fname bs es subs'
    | otherwise = throwError $ Fallback $ "could not match " ++ (show e) ++ " agains a list or string"
subsBinding fname (_:bs) (e:es) ex = subsBinding fname bs es ex

-- |Subsitute a variable in an expression with another expression
subs :: Expr -> Name -> Expr -> ThrowError Expr
subs (AppExpr e1 e2) old new = do
    s1 <- (subs e1 old new)
    s2 <- (subs e2 old new)
    return $ AppExpr s1 s2
subs v@(VarExpr x) old new
    | old == x = return new -- replace because they really match
    | otherwise = return v
    -- OPT (subs e1 old new) >>= (\e1 -> (subs e2 old new) >>= (\e2 -> return $ AppExpr e1 e2))
subs le@(ListExpr es) old new = liftM ListExpr (mapM (\e -> subs e old new) es)
subs (LamExpr n e) old new 
    | n == old = return $ (LamExpr n e)
    | otherwise = do
        subs' <- (subs e old new)
        return $ LamExpr n subs'
subs (LetExpr name old_def old_expr) old new = do
    new_def <- subs old_def old new
    new_expr <- subs old_expr old new
    return $ LetExpr name new_def new_expr
{- OPT (subs old_def old new) >>= 
    (\new_def -> (subs old_expr old new) >>= 
        (\new_expr -> return $ LetExpr name new_def new_expr)
    )-}
subs (CondExpr old_if old_then old_else) old new = do
    new_if <- (subs old_if old new)
    new_then <- (subs old_then old new)
    new_else <- (subs old_else old new)
    return $ CondExpr new_if new_then new_else
{- OPT (subs old_if old new) >>= 
     (\new_if -> (subs old_then old new) >>= 
      (\new_then -> (subs old_else old new) >>=
       (\new_else -> return $ CondExpr new_if new_then new_else)
      )
      -}
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
