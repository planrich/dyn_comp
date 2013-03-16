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

data SymbolTable = SymbolTable (M.Map String Func)



isAtom :: Expr -> Bool
isAtom (VarExpr _) = True
isAtom (BoolExpr _) = True
isAtom (LitExpr _) = True
isAtom _ = False

getId :: Expr -> Expr
getId e@(VarExpr n) = e

getName :: Expr -> Name
getName (VarExpr n) = n
getName _ = ""

isId :: Expr -> Bool
isId (VarExpr _) = True
isId _ = False

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

eval :: Expr -> ThrowError Expr
eval (AppExpr f s) = apply f s
eval e 
    | isAtom e = return $ e -- cannot reduce further

apply :: Expr -> Expr -> ThrowError Expr
apply a@(AppExpr _ _) args = (eval a) >>= (\a -> apply a args)
apply (VarExpr n) args =
    case buin of
        Just b -> (eval args) >>= (\args -> return $ CurryExpr b args)
        Nothing -> undefined
  where
    buin = M.lookup n builtins
apply c@(CurryExpr f e1) e2 = (eval e2) >>= f e1
apply func args = undefined

subs :: Expr -> Expr -> Expr -> Expr
subs a x e
    | isId e = if e == x then a else e
    | isApp e = AppExpr (subs a x (getFunction e)) (subs a x (getArgument e))
    | otherwise = let y = getId e in let c = getBody e in
        if y == x 
          then e 
          else let z = (VarExpr "u'") in LamExpr z (subs a x (subs z y c))

isBuiltin :: Expr -> Bool
isBuiltin (VarExpr n) = check (M.lookup n builtins)
  where
    check :: Maybe Builtin -> Bool
    check (Just b) = True
    check _ = False

builtins :: M.Map String Builtin
builtins = M.fromList [("add", numericBinary (+))
                      ,("sub", numericBinary (-))
                      ,("div", numericBinary div)
                      ,("mul", numericBinary (*))
                      ,("mod", numericBinary mod)
                      ,("and", boolBinary (&&))
                      ,("or", boolBinary (||))
                      ]

boolBinary :: (Bool -> Bool -> Bool) -> Expr -> Expr -> ThrowError Expr
boolBinary op (BoolExpr a1) (BoolExpr a2) = return $ BoolExpr $ op a1 a2
boolBinary op e1@(BoolExpr a1) e2 = (eval e2) >>= boolBinary op e1 
boolBinary op e2 e1@(BoolExpr a1) = (eval e1) >>= (\e1 -> boolBinary op e1 e2)
boolBinary op e1 _ = throwError $ TypeMissmatch "expected bool" e1 

numericBinary :: (Integer -> Integer -> Integer) -> Expr -> Expr -> ThrowError Expr
numericBinary op (LitExpr a1) (LitExpr a2) = return $ LitExpr $ op a1 a2
numericBinary op e1@(LitExpr a1) e2 = (eval e2) >>= numericBinary op e1
numericBinary op e2 e1@(LitExpr a1) = (eval e1) >>= (\e1 -> numericBinary op e1 e2)
numericBinary op e1 _ = throwError $ TypeMissmatch "expected number" e1 

harvestSymbols :: [Func] -> SymbolTable -> SymbolTable
harvestSymbols [] s = s
harvestSymbols (f:fs) (SymbolTable m) = SymbolTable (M.insert (funcName f) f m)

mainExpr :: SymbolTable -> Maybe Expr
mainExpr (SymbolTable m) = do
    M.lookup "main" m >>= firstPat . funcPatterns
  where
    firstPat ((Pattern [] e):ps) = Just e
    firstPat _ = Nothing

