module ParserTypes
    ( Name
    , Program (..)
    , Func (..)
    , match
    , Type (..)
    , Pattern (..)
    , Expr (..)
    , Binding (..)
    , matchPattern
    , bindingName
    , funcArgCount
    , ThrowError
    , EvalError (..)
    , Builtin (..)
    )
  where

import Data.Maybe
import Control.Monad.Error

type ThrowError = Either EvalError
type Name = String

data Program = Program { programFunctions :: [Func] }
             deriving (Show)

data Func = Func { funcName :: Name
                 , funcTypes :: [Type]
                 , funcPatterns :: [Pattern]
                 }
          deriving (Show)

data Pattern = Pattern { patternBindings :: [Binding]
                       , patternExpr :: Expr
                       }
             deriving (Show)

data Expr = AppExpr Expr Expr
          | LamExpr Name Expr
          | VarExpr Name
          | LitExpr Integer
          | ListExpr [Expr]
          | BoolExpr Bool
          | StrExpr String
          | CondExpr Expr Expr Expr
          -- |The function context might be the most different expression from the others.
          -- It should be created when a application is found that found a builtin (+,-,*,/,...)
          -- with the parameter. When going back the recursion the parameters are saved and
          -- when the param count the builtin needs is reached the function is executed.
          | FuncCtx Builtin [Expr]

data Binding = BNumber Int
             | BString String
             | BBool Bool
             | BAnon
             | BVar String
             | BList (Binding,Binding)
             deriving (Show)

data Builtin = Builtin { builtinParamCount :: Int
                       , builtinFunction :: ([Expr] -> ThrowError Expr)
                       }
             | Defined { definedParamCount :: Int
                       , definedFunction :: Func
                       }

data Type = TInt
          | TString
          | TList Type
          deriving Show

data EvalError = TypeMissmatch String Expr
               | InvalidArgument String
               | SymbolNotFound String
               | PatternFallthrough String [Expr]
               | ConditionalNotABool Expr
               | CannotApply Expr Expr
               | Fallback String

instance Show EvalError where
    show (TypeMissmatch msg e) = "type missmatch: " ++ msg ++ " got: " ++ (show e) 
    show (InvalidArgument msg) = "invalid argument: " ++ msg
    show (SymbolNotFound msg) = "symbol '" ++ msg ++ "' not found"
    show (PatternFallthrough funcname exprs) = "could not match '" ++ (show exprs) ++ "' with any of the patterns from " ++ funcname
    show (Fallback msg) = "error: " ++ msg
    show (ConditionalNotABool e) = "fact did not evaluate to true or false: " ++ (show e)
    show (CannotApply e1 e2) = "cannot apply these two expressions. 1: " ++ (show e1) ++ ". 2: " ++ (show e2)

instance Error EvalError where
    noMsg = Fallback "error"
    strMsg = Fallback

instance Show Expr where
    show (AppExpr e1 e2) = " #(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (LitExpr i) = "$(" ++ (show i) ++ ")"
    show (VarExpr n) = "\"" ++ n ++ "\""
    show (BoolExpr True) = "*(t)"
    show (BoolExpr False) = "*(f)"
    show (ListExpr (ls)) = "[" ++ (foldr (++) "" (map ((++ ",") . show) ls)) ++ "]"
    show (CondExpr e a1 a2) = "{?" ++ (show e) ++ " either " ++ (show a1) ++ " or " ++ (show a2) ++ "}"
    show (LamExpr name expr) = "\\" ++ name ++ " -> " ++ (show expr)
    show (FuncCtx (Defined n f) expr) = "@(defined: " ++ funcName f ++ " " ++ (show expr) ++ ")"
    show (FuncCtx (Builtin n f) expr) = "@(builtin: " ++ (show expr) ++ ")"

instance Eq Expr where
    (==) (AppExpr e1 e2) (AppExpr e3 e4) = e1 == e3 && e2 == e4
    (==) (LitExpr i) (LitExpr i2) = i == i2
    (==) (VarExpr n) (VarExpr n2) = n == n2
    (==) (BoolExpr b) (BoolExpr b2) = b == b2
    (==) (ListExpr ll) (ListExpr lr) = ll == lr
    (==) _ _ = False

bindingName :: Binding -> Maybe String
bindingName (BVar s) = Just s
bindingName _ = Nothing

match :: [Expr] -> [Pattern] -> Maybe Pattern
match es [] = Nothing
match es (p:ps) = mplus (if isJust $ matchPattern p es then Just p else Nothing) (match es ps) 

matchPattern :: Pattern -> [Expr] -> Maybe Pattern
matchPattern p [] = Just p
matchPattern (Pattern (b:bs) expr) (e:es)
    | matchBinding b e = matchPattern (Pattern bs expr) es
    | otherwise = Nothing
matchPattern _ _ = Nothing

--firstPat ((Pattern [] e):ps) = Just e
--firstPat _ = Nothing

matchBinding :: Binding -> Expr -> Bool
matchBinding BAnon _ = True
matchBinding (BVar _) _ = True
matchBinding (BNumber b) (LitExpr l) = b == fromIntegral l
matchBinding (BBool b1) (BoolExpr b2) = b1 == b2
matchBinding _ _ = False

-- |How many arguments must a specific function get to be executed?
--  It is assumed that every pattern of a function has the
--  same amount of bindings.
funcArgCount :: Func -> Int
funcArgCount (Func name types (p:ps)) = patternArgCount p

patternArgCount :: Pattern -> Int
patternArgCount (Pattern bindings _) = length bindings


