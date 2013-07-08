module IR.Quadrupel
<<<<<<< HEAD
    ( transform
    )
  where



import ParserTypes

import IR.QuadrupelTypes

import Control.Monad.Writer
import Control.Monad.State

import qualified Data.Map as M

data QuadrupelCode = QuadrupelCode { qrcCode :: [Quadrupel] }

type QCGen = State QuadrupelCode ()

transform :: Program -> QR
transform (Program unit funcs) = QRUnit unit (map simpleTransform funcs)

simpleTransform :: Func -> QR
simpleTransform (Func name _ []) = QRFunc name []
simpleTransform (Func name _ (p:_)) = 
    let (_,qs) = runState (simpleTransformExpr (patternExpr p)) (QuadrupelCode []) in QRFunc name (qrcCode qs)

simpleTransformExpr :: Expr -> QCGen
simpleTransformExpr (AppExpr2 (fun:params)) = 
    undefined
  where
    paramCount = length params
    functionType = 
simpleTransformExpr (LitExpr i) = do
    modify $ addQC (QRAssign (Target "") (Value (fromIntegral i)))
simpleTransformExpr _ = do
    modify (\x -> QuadrupelCode $ (qrcCode x) ++ [QRAssign (Target "test") (Value 1)])


addQC :: Quadrupel -> QuadrupelCode -> QuadrupelCode
addQC q = (\x -> QuadrupelCode $ (qrcCode x) ++ [q])

simpleApply :: Expr -> [Expr] -> QCGen
simpleApply (AppExpr f s) l  = 
    simpleApply f (l ++ [s])
simpleApply (VarExpr name) params = return ()
{-
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


-}




