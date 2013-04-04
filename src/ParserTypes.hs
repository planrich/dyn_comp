module ParserTypes
    ( Name
    , Program (..)
    , Func (..)
    , match
    , Type (..)
    , Pattern (..)
    , Expr (..)
    , Binding (..)
    , bindingName
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

match :: [Expr] -> [Pattern] -> Maybe Pattern
match es [] = Nothing
match es (p:ps) = mplus (if isJust $ matchPattern p es then Just p else Nothing) (match es ps) 

matchPattern :: Pattern -> [Expr] -> Maybe Pattern
matchPattern p [] = Just p
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

data Builtin = Builtin { builtinParamCount :: Int
                       , builtinFunction :: ([Expr] -> ThrowError Expr)
                       }

data Expr = AppExpr Expr Expr
          | LamExpr Expr Expr
          | VarExpr Name
          | LitExpr Integer
          | ListExpr [Expr]
          | BoolExpr Bool
          | StrExpr String
          -- |The function context might be the most different expression from the others.
          -- It should be created when a application is found that found a builtin (+,-,*,/,...)
          -- with the parameter. When going back the recursion the parameters are saved and
          -- when the param count the builtin needs is reached the function is executed.
          | FuncCtx Builtin [Expr]
          -- |deprecated
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

bindingName :: Binding -> Maybe String
bindingName (BVar s) = Just s
bindingName _ = Nothing

data Type = TInt
          | TString
          | TList Type
          deriving Show

data EvalError = TypeMissmatch String Expr
               | InvalidArgument String
               | SymbolNotFound String
               | PatternFallthrough String [Expr]
               | Fallback String

instance Show EvalError where
    show (TypeMissmatch msg e) = "type missmatch: " ++ msg ++ " got: " ++ (show e) 
    show (InvalidArgument msg) = "invalid argument: " ++ msg
    show (SymbolNotFound msg) = "symbol '" ++ msg ++ "' not found"
    show (PatternFallthrough funcname exprs) = "could not match '" ++ (show exprs) ++ "' with any of the patterns from " ++ funcname
    show (Fallback msg) = "error: " ++ msg

instance Error EvalError where
    noMsg = Fallback "error"
    strMsg = Fallback

type ThrowError = Either EvalError

