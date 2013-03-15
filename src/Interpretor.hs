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

type Builtin = (Expr -> Expr -> Expr)

isAtom :: Expr -> Bool
isAtom (VarExpr _) = True
isAtom (LitExpr _) = True
isAtom _ = False

getId :: Expr -> Name
getId (VarExpr n) = n
getId _ = ""

isFunc :: Expr -> Bool
isFunc (LamExpr _ _) = True
isFunc _ = False

isApp :: Expr -> Bool
isApp (AppExpr _) = True
isApp _ = False

getFunc :: Expr -> Expr
getFunc l@(LamExpr _ _) = l
getFunc v@(VarExpr n) = v



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
eval (AppExpr []) = undefined
eval (AppExpr (f@(VarExpr _):as)) = apply f as
eval e 
    | isAtom e = e -- cannot reduce further

apply :: Expr -> [Expr] -> Expr
apply func args
    | isBuiltin func = applyBuiltin (M.lookup (getId func) builtins) args
    | isApp func = apply (eval func) args
    | otherwise = undefined


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

