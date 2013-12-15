module ParserTypes
    ( ThrowError
    , EvalError (..)
    , Builtin (..)
    , match
    , matchPattern
    , bindingName
    , isListExpr
    , listHead
    , listTail
    )
  where

import Data.Maybe
import Control.Monad.Error

import Neart.Types

type ThrowError = Either EvalError
type Name = String

data Builtin = Builtin { builtinParamCount :: Int
                       , builtinFunction :: ([Expr] -> ThrowError Expr)
                       }
             | Defined { definedParamCount :: Int
                       , definedFunction :: Function Expr
                       }

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
    noMsg = Fallback "no message provided"
    strMsg = Fallback

instance Show Expr where
    show (AppExpr e1 e2) = " #(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
    show (LitExpr i) = (show i)
    show (Expr es) = " %" ++ (show es)
    show (VarExpr n) = "\"" ++ n ++ "\""
    show (CharExpr c) = ('\'':(c:"'"))
    show (StrExpr s) = ('"':s) ++ "\""
    show (BoolExpr True) = "*(t)"
    show (BoolExpr False) = "*(f)"
    show (ListExpr (ls)) = "[" ++ (foldr (++) "" (map ((++ ",") . show) ls)) ++ "]"
    show (CondExpr e a1 a2) = "{?" ++ (show e) ++ " either " ++ (show a1) ++ " or " ++ (show a2) ++ "}"
    show (LamExpr name expr) = "\\" ++ name ++ " -> " ++ (show expr)
    show (FuncCtx (Defined _ f) expr) = "@(defined: " ++ functionName f ++ " " ++ (show expr) ++ ")"
    show (FuncCtx (Builtin _ _) expr) = "@(builtin: " ++ (show expr) ++ ")"
    show _ = "cannot show expr!!!"

instance Ord Expr where
    compare (LitExpr i) (LitExpr j) = compare i j
    compare e1 e2 = error ("tried to compare " ++ (show e1) ++ " with " ++ (show e2))

instance Eq Expr where
    (==) (AppExpr e1 e2) (AppExpr e3 e4) = e1 == e3 && e2 == e4
    (==) (LitExpr i) (LitExpr i2) = i == i2
    (==) (VarExpr n) (VarExpr n2) = n == n2
    (==) (BoolExpr b) (BoolExpr b2) = b == b2
    (==) (ListExpr ll) (ListExpr lr) = ll == lr
    (==) (FuncCtx (Defined ac1 f1) p1) (FuncCtx (Defined ac2 f2) p2) =
        ac1 == ac2 && (functionArgumentCount f1) == (functionArgumentCount f2) && (functionName f1) == (functionName f2) && p1 == p2
    (==) _ _ = False

isBindingVar :: Binding -> Bool
isBindingVar (BVar _) = True
isBindingVar _ = False

bindingName :: Binding -> Maybe String
bindingName (BVar s) = Just s
bindingName _ = Nothing

match :: [Expr] -> [Pattern Expr] -> Maybe (Pattern Expr)
match es [] = Nothing
match es (p:ps) = mplus (if isJust $ matchPattern p es then Just p else Nothing) (match es ps) 

matchPattern :: Pattern Expr -> [Expr] -> Maybe (Pattern Expr)
matchPattern p [] = Just p
matchPattern (Pattern (b:bs) expr) (e:es)
    | matchBinding b e = matchPattern (Pattern bs expr) es
    | otherwise = Nothing
matchPattern _ _ = Nothing

matchBinding :: Binding -> Expr -> Bool
matchBinding BAnon _ = True
matchBinding (BVar _) _ = True
matchBinding (BNumber b) (LitExpr l) = b == fromIntegral l
matchBinding (BBool b1) (BoolExpr b2) = b1 == b2
matchBinding (BList (bh,bt)) ex
    | isListExpr ex && listNotEmpty ex = 
        if matchBinding bh (listHead ex) && matchBinding bt (listTail ex)
        then True
        else False
    | otherwise = False
matchBinding BNil (ListExpr []) = True
matchBinding _ _ = False

listNotEmpty :: Expr -> Bool
listNotEmpty (ListExpr []) = False
listNotEmpty (ListExpr _) = True
listNotEmpty (StrExpr []) = False
listNotEmpty (StrExpr _) = True
listNotEmpty _ = False

isListExpr :: Expr -> Bool
isListExpr (ListExpr _) = True
isListExpr (StrExpr _) = True
isListExpr _ = False

listHead :: Expr -> Expr
listHead (ListExpr (e:_)) = e
listHead (StrExpr (e:_)) = CharExpr e
listHead e = error $ "called listHead on " ++ (show e)

listTail :: Expr -> Expr
listTail (ListExpr (_:es)) = ListExpr es
listTail (StrExpr (_:es)) = StrExpr es
listTail e = error $ "called listTail on " ++ (show e)

