module Interpretor 
    ( eval
    , SymbolTable (..)
    , harvestSymbols
    , mainExpr
    )
  where

import qualified Data.Map as M
import ParserTypes

data SymbolTable = SymbolTable (M.Map String Func)


isAtom :: Expr -> Bool
isAtom (VarExpr _) = True
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

--getFunc :: Expr -> Expr
--getFunc l@(LamExpr _ _) = l
--getFunc v@(VarExpr n) = v



-- (AppExpr 
--      (AppExpr 
--          (VarExpr "add") 
--          (LitExpr 0)
--      ) 
--      (AppExpr 
--          (AppExpr 
--              (VarExpr "add") 
--              (LitExpr 1)
--          ) 
--      (LitExpr 2)
--      )
-- )
eval :: Expr -> Expr
--eval (AppExpr []) = undefined
eval (AppExpr f s) = apply f s
eval e 
    | isAtom e = e -- cannot reduce further

apply :: Expr -> Expr -> Expr
apply a@(AppExpr _ _) args = apply (eval a) args
apply (VarExpr n) args =
    case buin of
        Just b -> CurryExpr b (eval args)
        Nothing -> undefined
  where
    buin = M.lookup n builtins
apply c@(CurryExpr f e1) e2 = f e1 (eval e2)
apply func args = undefined

applyCurry :: Builtin -> Expr -> Expr -> Expr
applyCurry f e1 e2 = f e1 e2

subs :: Expr -> Expr -> Expr -> Expr
subs a x e
    | isId e = if e == x then a else e
    | isApp e = AppExpr (subs a x (getFunction e)) (subs a x (getArgument e))
    | otherwise = let y = getId e in let c = getBody e in
        if y == x 
          then e 
          else let z = (VarExpr "u'") in LamExpr z (subs a x (subs z y c))


applyBuiltin :: Maybe Builtin -> [Expr] -> Expr
applyBuiltin (Just f) es = foldl1 f (map eval es)

numericBinary :: (Integer -> Integer -> Integer) -> Expr -> Expr -> Expr
numericBinary op (LitExpr a1) (LitExpr a2) = LitExpr $ op a1 a2
numericBinary op e1@(LitExpr a1) e2 = numericBinary op e1 (eval e2)
numericBinary op e2 e1@(LitExpr a1) = numericBinary op (eval e1) e2
numericBinary op _ _ = undefined

isBuiltin :: Expr -> Bool
isBuiltin (VarExpr n) = check (M.lookup n builtins)
  where
    check :: Maybe Builtin -> Bool
    check (Just b) = True
    check _ = False

builtins :: M.Map String (Expr -> Expr -> Expr)
builtins = M.fromList [("add", numericBinary (+))
                    ,("sub", numericBinary (-))
                    ,("div", numericBinary div)
                    ,("mul", numericBinary (*))
                    ,("mod", numericBinary mod)
                    ]

harvestSymbols :: [Func] -> SymbolTable -> SymbolTable
harvestSymbols [] s = s
harvestSymbols (f:fs) (SymbolTable m) = SymbolTable (M.insert (funcName f) f m)

mainExpr :: SymbolTable -> Maybe Expr
mainExpr (SymbolTable m) = do
    M.lookup "main" m >>= firstPat . funcPatterns
  where
    firstPat ((Pattern [] e):ps) = Just e
    firstPat _ = Nothing

