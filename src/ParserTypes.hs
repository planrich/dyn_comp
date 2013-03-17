module ParserTypes
    ( Name
    , Program (..)
    , Func (..)
    , match
    , Type (..)
    , Pattern (..)
    , Expr (..)
    , Binding (..)
    , Builtin (..)
    , ThrowError
    , EvalError (..)
    )
  where

import Data.Maybe
import Control.Monad.Error

type Name = String

data Program = Program { programFunctions :: [Func] }
             deriving (Show)

data Func = Func { funcName :: Name
                 , funcTypes :: [Type]
                 , funcPatterns :: [Pattern]
                 }
          deriving (Show)

match :: [Expr] -> [Pattern] -> Maybe Expr
match es [] = Nothing
match es (p:ps) = maybe (match es ps) ((flip matchPattern) es) $ Just p

matchPattern :: Pattern -> [Expr] -> Maybe Expr
matchPattern (Pattern [] expr) [] = Just expr
matchPattern (Pattern (b:bs) expr) (e:es)
    | matchBinding b e = matchPattern (Pattern bs expr) es
    | otherwise = Nothing
matchPattern _ _ = Nothing

matchBinding :: Binding -> Expr -> Bool
matchBinding BAnon _ = True
matchBinding (BVar _) _ = True
matchBinding (BNumber b) (LitExpr l) = b == fromIntegral l


data Pattern = Pattern [Binding] Expr
             deriving (Show)

data Builtin = Builtin Int ([Expr] -> ThrowError Expr)

data Expr = AppExpr Expr Expr
          | LamExpr Expr Expr
          | VarExpr Name
          | LitExpr Integer
          | ListExpr [Expr]
          | BoolExpr Bool
          | StrExpr String
          | CurryExpr Builtin Expr

instance Show Expr where
    show (AppExpr e1 e2) = " #(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (LitExpr i) = "$(" ++ (show i) ++ ")"
    show (VarExpr n) = "\"" ++ n ++ "\""
    show (BoolExpr True) = "*(t)"
    show (BoolExpr False) = "*(f)"
    show (CurryExpr b e) = "@(" ++ (show e) ++ ")"
    show (ListExpr (ls)) = "[" ++ (foldr (++) "" (map ((++ ",") . show) ls)) ++ "]"

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
               | InvalidArgument String
               | Fallback String

instance Show EvalError where
    show (TypeMissmatch msg e) = "type missmatch: " ++ msg ++ " got: " ++ (show e) 
    show (InvalidArgument msg) = "invalid argument: " ++ msg
    show (Fallback msg) = "error: " ++ msg

instance Error EvalError where
    noMsg = Fallback "error"
    strMsg = Fallback

type ThrowError = Either EvalError

