module IR.Quadrupel
    ( Operation (..)
    , Quadrupel (..)
    , transform
    , TEnv (..)
    )
  where

import ParserTypes
import Environment
import Builtins

import Control.Monad.State

type Reg = Integer


data Operation 
    = Add
    | Mul
    | Div
    | Sub
  deriving (Show)


data Quadrupel =
    Quadrupel 
        { targetReg :: Reg
        , operation :: Operation
        , op1Reg :: Reg
        , op2Reg :: Reg
        }
      deriving (Show)

data TEnv = TEnv
    { code :: [Quadrupel]
    , symTable :: SymbolTable
    , registerVar :: Reg
    }
  deriving (Show)

type QCT a = StateT TEnv IO a

type Module = String

data Symbol = FuncSym Module Name
    deriving (Show)

transform :: Expr -> QCT ()
transform (Expr ((VarExpr fname):es)) = do
    mSym <- findSymbol fname
    tenv <- get
    lift . putStrLn $ fname ++ " " ++ (show mSym)
    return ()

findSymbol :: Name -> QCT (Maybe Symbol)
findSymbol name = do
    tenv <- get
    return $
      mplus 
        (liftM (\_ -> FuncSym "" name) (findFunc (symTable tenv) name))
        (liftM (\_ -> FuncSym "builtin" name) (findBuiltin name))

apply :: Symbol -> [Expr] -> QCT ()
apply (FuncSym m n) params = undefined
    -- determin dependencies














