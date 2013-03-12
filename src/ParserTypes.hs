module ParserTypes
    ( Name
    , Program (..)
    , Func (..)
    , Type (..)
    , Pattern (..)
    , Expr (..)
    , Binding (..)
    )
  where

type Name = String

data Program = Program [Func]
             deriving (Show)

data Func = Func Name [Type] [Pattern]
          deriving (Show)

data Pattern = Pattern [Binding] Expr
             deriving (Show)

data Expr = AppExpr Expr Expr
          | LitExpr Integer
          | VarExpr String
          deriving (Show)

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




