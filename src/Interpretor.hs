module Interpretor 
    ( eval
    , SymbolTable (..)
    , harvestSymbols
    , mainExpr
    )
  where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Error

import ParserTypes

type Env = SymbolTable

eval :: Env -> Expr -> ThrowError Expr
eval env (AppExpr f s) = apply env f s
eval env (ListExpr es) = liftM ListExpr $ mapM (eval env) es
eval env e
    | isAtom e = return $ e -- cannot reduce further
    | otherwise =  throwError $ Fallback (show e)

apply :: Env -> Expr -> Expr -> ThrowError Expr
apply env a@(AppExpr _ _) args = (eval env a) >>= (\a -> apply env a args)
apply env c@(CurryExpr f e1) (VarExpr n) =
    case M.lookup n builtins of
        Just b -> return $ CurryExpr b c
        Nothing -> undefined -- must be bound variable
apply env c1@(CurryExpr f1 e1) c2@(CurryExpr f2 e2) = undefined
apply env c@(CurryExpr f e) e2
    | isCurry e = (applyCurry e e2)
    | otherwise = 
        case f of
            Builtin 2 f -> f [e,e2]
            Builtin n f -> undefined
apply env (VarExpr n) args =
    case M.lookup n builtins of
        Just (Builtin 1 f) -> (eval env args) >>= (\args -> f [args])
        Just b -> (eval env args) >>= (\args -> return $ CurryExpr b args)
        Nothing -> applyFromEnv env (VarExpr n) args
apply env func args = undefined

applyFromEnv :: Env -> Expr -> Expr -> ThrowError Expr
applyFromEnv 
            apply
            sym <- findEntry env
            if isJust sym
              then match [args] (funcPatterns . fromJust sym)
              else throwError

applyCurry :: Expr -> Expr -> ThrowError Expr
applyCurry (CurryExpr f e) param
    | isCurry e = (applyCurry e param)
    | otherwise = -- f [e, param]
        case f of
            Builtin 2 f -> f [e,param]
            Builtin n f -> undefined

subs :: Expr -> Expr -> Expr -> Expr
subs a x e
    | isId e = if e == x then a else e
    | isApp e = AppExpr (subs a x (getFunction e)) (subs a x (getArgument e))
    | otherwise = let y = getId e in let c = getBody e in
        if y == x 
          then e 
          else let z = (VarExpr "u'") in LamExpr z (subs a x (subs z y c))

existsBuiltin :: Int -> String -> Maybe Builtin
existsBuiltin argcount name =
    case M.lookup name builtins of
        Just bi@(Builtin argcount' b) -> if argcount' == argcount then Just bi else Nothing
        _ -> Nothing

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
                      , ("head", Builtin 1 $ extractListOp 1 (head))
                      --, ("tail", Builtin 1 extractListOp (tail))
                      ]

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

harvestSymbols :: [Func] -> SymbolTable -> SymbolTable
harvestSymbols [] s = s
harvestSymbols (f:fs) (SymbolTable m) = SymbolTable (M.insert (funcName f) f m)

mainExpr :: SymbolTable -> Maybe Expr
mainExpr (SymbolTable m) = do
    M.lookup "main" m >>= firstPat . funcPatterns
  where
    firstPat ((Pattern [] e):ps) = Just e
    firstPat _ = Nothing

type SymEntry = Func
data SymbolTable = SymbolTable (M.Map String SymEntry)

newSymT :: SymbolTable 
newSymT = SymbolTable M.empty

defineSym :: SymbolTable -> Name -> SymEntry -> SymbolTable
defineSym (SymbolTable t) k s = SymbolTable $ M.insert k s t

findEntry :: SymbolTable -> Name -> Maybe SymEntry
findEntry sym@(SymbolTable t) k = M.lookup k t

isAtom :: Expr -> Bool
isAtom (VarExpr _) = True
isAtom (BoolExpr _) = True
isAtom (LitExpr _) = True
isAtom (ListExpr _) = True
isAtom _ = False

getId :: Expr -> Expr
getId e@(VarExpr n) = e

getName :: Expr -> Name
getName (VarExpr n) = n
getName _ = ""

isId :: Expr -> Bool
isId (VarExpr _) = True
isId _ = False

isCurry :: Expr -> Bool
isCurry (CurryExpr _ _) = True
isCurry _ = False

isFunc :: Expr -> Bool
isFunc (LamExpr _ _) = True
isFunc _ = False

getFunction :: Expr -> Expr
getFunction (AppExpr e e1) = getFunction e
getFunction l@(LamExpr _ _) = l

getArgument :: Expr -> Expr
getArgument (AppExpr _ e) = e

getBody :: Expr -> Expr
getBody (LamExpr _ b) = b

isApp :: Expr -> Bool
isApp (AppExpr _ _) = True
isApp _ = False

