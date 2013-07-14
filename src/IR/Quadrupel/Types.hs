module IR.Quadrupel.Types
  ( Reg
  , Operation (..)
  , QUnit (..)
  , QFunction (..)
  , QBlock (..)
  , BinOp (..)
  , Operand (..)
  , Quadrupel (..)
  , TEnv (..)
  , QuadrupelCode (..)
  , TransformError (..)
  , Symbol (..)
  , emptyTransEnv
  , tenvSetSymTable
  , tenvPushQuadrupel
  , tenvIncReg
  , tenvResetCode
  , isConstant
  , builtinMap
  )
 where

import ParserTypes
import Environment

import Control.Monad.Trans.Error

import qualified Data.Map as M

type Reg = Integer
type Module = String

data Operation = Binary BinOp
               | Unary
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

data QBlock = QBlock
    { qblockLabel :: Name
    , qblockCode :: [Quadrupel]
    }
  deriving (Show)

data QFunction = QFunction
    { qfuntionLabel :: Name
    , qfunctionName :: Name
    , qfunctionBlocks :: [QBlock]
    }
  deriving (Show)

data QUnit = QUnit
    { qunitMeta :: MetaUnit
    , qunitFunctions :: [QFunction]
    }
  deriving (Show)

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
    , tenvUnit :: Unit
    }
  deriving (Show)
data QuadrupelCode = QCode { qcode :: [Quadrupel]
                           , qcodeResult :: Operand
                           }
                         deriving (Show)

data TransformError = SymbolNotFound String Expr 
                    | DefaultError String
                  deriving (Show)

instance Error TransformError where
    noMsg = DefaultError ""
    strMsg msg = DefaultError msg

data Symbol = FuncSym Module Name
            | CoreFunc Operation
    deriving (Show)

builtinMap = M.fromList 
             [ ("add", Binary Add)
             , ("mul", Binary Mul)
             , ("div", Binary Div)
             , ("sub", Binary Sub)
             ]

isConstant :: Operand -> Bool
isConstant (Constant i) = True
isConstant _ = False

emptyTransEnv :: SymbolTable -> Unit -> TEnv
emptyTransEnv symTable = TEnv [] symTable 0

tenvPushQuadrupel :: Quadrupel -> TEnv -> TEnv
tenvPushQuadrupel q (TEnv code table reg u) = TEnv (code ++ [q]) table reg u

tenvIncReg :: TEnv -> TEnv
tenvIncReg (TEnv c t reg u) = (TEnv c t (reg + 1) u)

tenvResetCode :: TEnv -> TEnv
tenvResetCode (TEnv code t r u) = TEnv [] t r u

tenvSetSymTable :: SymbolTable -> TEnv -> TEnv
tenvSetSymTable symt (TEnv c _ r u) = TEnv c symt r u


