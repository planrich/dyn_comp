module IR.Expr.Types
    ( Expr (..)
    )

data Expr = AppExpr Expr Expr
          | PrefixExpr [Expr]
          | LamExpr Name Expr
          | VarExpr Name
          | LitExpr Int
          | ListExpr ![Expr]
          | BoolExpr Bool
          | CharExpr Char
          | StrExpr String
          | CondExpr Expr Expr Expr
          -- | define a common sub expression
          | LetExpr Name Expr Expr
          -- | The function context might be the most different expression from the others.
          -- It should be created when a application is found that found a builtin (+,-,*,/,...)
          -- with the parameter. When going back the recursion the parameters are saved and
          -- when the param count the builtin needs is reached the function is executed.
          -- DEPRECATED
          | FuncCtx Builtin [Expr]

