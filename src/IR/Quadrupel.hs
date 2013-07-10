module IR.Quadrupel
    ( Operation (..)
    , Quadrupel (..)
    , transform
    , TEnv (..)
    , prettyPrint
    )
  where

import ParserTypes
import Environment
import Builtins

import Control.Monad.State
import Control.Monad.Trans.Error

import qualified Data.Map as M

type Reg = Integer

data Operation = Binary BinOp
  deriving (Show)

data BinOp = Add
           | Sub
           | Mul
           | Div

instance Show BinOp where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"


builtinMap = M.fromList 
             [ ("add", Binary Add)
             , ("mul", Binary Mul)
             , ("div", Binary Div)
             , ("sub", Binary Sub)
             ]

data Operand = Register Reg
             | Constant Integer

instance Show Operand where
    show (Register r) = "r" ++ (show r)
    show (Constant i) = (show i)

data Quadrupel =
    Quadrupel 
        { targetReg :: Reg
        , operation :: Operation
        , op1Reg :: Operand
        , op2Reg :: Operand
        }
      deriving (Show)

data TEnv = TEnv
    { code :: [Quadrupel]
    , symTable :: SymbolTable
    , registerVar :: Reg
    }
  deriving (Show)

appendQuadrupel :: Quadrupel -> TEnv -> TEnv
appendQuadrupel q (TEnv code table reg) = TEnv (code ++ [q]) table reg

incReg :: TEnv -> TEnv
incReg (TEnv c t reg) = (TEnv c t (reg + 1))

data TransformError = SymbolNotFound String Expr 
                    | DefaultError String
                  deriving (Show)

instance Error TransformError where
    noMsg = DefaultError ""
    strMsg msg = DefaultError msg

type GQCT e s a = ErrorT e (StateT s IO) a
type QCT a = GQCT TransformError TEnv a

type Module = String

data Symbol = FuncSym Module Name
            | CoreFunc Operation
    deriving (Show)

failTransform :: TransformError -> QCT Operand
failTransform e = throwError e

transform :: Expr -> QCT Operand
transform (LitExpr i) = return $ Constant $ fromIntegral i
transform expr@(Expr ((VarExpr fname):es)) = do
    tenv <- get
    mSym <- return $ findSymbol tenv fname
    case mSym of
        Just sym -> apply sym es
        Nothing -> failTransform $ IR.Quadrupel.SymbolNotFound ("could not find symbol '" ++ fname ++ "'") expr

findSymbol :: TEnv -> Name -> Maybe Symbol
findSymbol tenv name = do
    mplus (liftM (\_ -> FuncSym "" name) (findFunc (symTable tenv) name)) 
          (liftM (\op -> CoreFunc op) (M.lookup name builtinMap))

apply :: Symbol -> [Expr] -> QCT Operand
apply (FuncSym m n) params = undefined
apply (CoreFunc op) params = do
    results <- mapM transform params
    coreFunc op results

pushInstr :: Quadrupel -> QCT ()
pushInstr q = modify (appendQuadrupel q)

uniqueReg :: QCT Reg
uniqueReg = do
    tenv <- get
    put (incReg tenv)
    return (registerVar tenv)

coreFunc :: Operation -> [Operand] -> QCT Operand
coreFunc op@(Binary _) (op1:op2:[]) = do
    reg <- uniqueReg
    pushInstr $ Quadrupel reg op op1 op2
    return $ Register reg

prettyPrint :: Quadrupel -> IO ()
prettyPrint (Quadrupel r (Binary op) op1 op2) = 
    putStrLn $ "r" ++ (show r) ++ " = " ++ (show op1) ++ " " ++ (show op) ++ " " ++ (show op2)




