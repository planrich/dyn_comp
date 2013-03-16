module ParserTypes
    ( Name
    , Program (..)
    , Func (..)
    , Type (..)
    , Pattern (..)
    , Expr (..)
    , Binding (..)
    , Builtin (..)
    , ThrowError
    , EvalError (..)
    )
  where

import Control.Monad.Error

type Name = String

data Program = Program { programFunctions :: [Func] }
             deriving (Show)

data Func = Func { funcName :: Name
                 , funcTypes :: [Type]
                 , funcPatterns :: [Pattern]
                 }
          deriving (Show)

data Pattern = Pattern [Binding] Expr
             deriving (Show)

type Builtin = (Expr -> Expr -> ThrowError Expr)

data Expr = AppExpr Expr Expr
          | LitExpr Integer
          | BoolExpr Bool
          | StrExpr String
          | LamExpr Expr Expr
          | VarExpr Name
          | CurryExpr Builtin Expr

instance Show Expr where
    show (AppExpr e1 e2) = " #(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (LitExpr i) = "$(" ++ (show i) ++ ")"
    show (VarExpr n) = "\"" ++ n ++ "\""
    show (CurryExpr b e) = "@(" ++ (show e) ++ ")"

instance Eq Expr where
    (==) (AppExpr e1 e2) (AppExpr e3 e4) = e1 == e3 && e2 == e4
    (==) (LitExpr i) (LitExpr i2) = i == i2
    (==) (VarExpr n) (VarExpr n2) = n == n2
    (==) (BoolExpr b) (BoolExpr b2) = b == b2
    (==) _ _ = False

data Binding = BNumber Int
             | BString String
             | BAnon
             | BVar String
             | BList (Binding,Binding)
             deriving (Show)


data Type = TInt
          | TString
          | TList Type
          deriving Show

data EvalError = TypeMissmatch String Expr
               | Fallback String

instance Show EvalError where
    show (TypeMissmatch msg e) = "type missmatch: " ++ msg ++ " got: " ++ (show e) 
    show (Fallback msg) = "error: " ++ msg

instance Error EvalError where
    noMsg = Fallback "error"
    strMsg = Fallback

type ThrowError = Either EvalError

