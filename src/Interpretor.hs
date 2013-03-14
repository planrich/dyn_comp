module Interpretor 
    ( eval
    , SymbolTable (..)
    , harvestSymbols
    , mainExpr
    )
  where

import qualified Data.Map as M
import ParserTypes

data BuiltIn = NaturalFunc (Int -> Int -> Int)

data SymbolTable = SymbolTable (M.Map String Func)

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
eval (AppExpr (VarExpr name) e2) = apply name e2
eval (AppExpr e1 e2) = eval e1 

apply :: String -> Expr -> Expr
apply func args = undefined


builtins :: M.Map String BuiltIn
builtins = M.fromList [("add", NaturalFunc (+))
                    ,("sub", NaturalFunc (-))
                    ,("div", NaturalFunc div)
                    ,("mul", NaturalFunc (*))
                    ,("mod", NaturalFunc mod)
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

