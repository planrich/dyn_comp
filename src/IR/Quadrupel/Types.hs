module IR.Quadrupel.Types
  ( Reg
  , Operation (..)
  , VirtualRegister (..)
  , QUnit (..)
  , QFunction (..)
  , QPattern (..)
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
  , tenvInsertSymbol
  , isConstant
  , builtinMap
  , prettyPrintQU
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

data QPattern = QPattern
    { qpatternLabel :: Name
    , qpatternCode :: [Quadrupel]
    }
  deriving (Show)

data QFunction = QFunction
    { qfunctionLabel :: Name
    , qfunctionName :: Name
    , qfunctionPatterns :: [QPattern]
    }
  deriving (Show)

data QUnit = QUnit
    { qunitMeta :: MetaUnit
    , qunitFunctions :: [QFunction]
    }
  deriving (Show)

data VirtualRegister = VRegister Reg
                     | ARegister Reg
                   deriving (Show)

data Operand r = OpRegister Register r
               | Constant Integer
               | Nil

class Register r where
    registerName :: r -> String

instance Register VirtualRegister where
    registerName (VRegister r) = "r" ++ (show r)
    registerName (ARegister r) = "a" ++ (show r)

instance Show Operand where
    show (OpRegister r) = (show r)
    show (Constant i) = (show i)
    show Nil = "nil"

data Quadrupel r = QAssignOp { targetReg :: Register r
                           , operation :: Operation
                           , op1Reg :: Operand r
                           , op2Reg :: Operand r
                           }
               | QAssign { targetReg :: Register r
                         , operand :: Operand r
                         }
               | QParam { paramIdx :: Int
                        , operand :: Operand r
                        }
               | QCall { qcallLabel :: Name } 
               | QReturn { qreturnOperand :: Operand r }
             deriving (Show)

data TEnv = TEnv
    { tenvCode :: [Quadrupel]
    , tenvSymTable :: SymbolTable
    , tenvRegisterVar :: Reg
    , tenvUnit :: Unit
    }
  deriving (Show)

data TransformError = SymbolNotFound String Expr 
                    | DefaultError String
                  deriving (Show)

instance Error TransformError where
    noMsg = DefaultError ""
    strMsg msg = DefaultError msg

data Symbol = FuncSym Name
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
tenvPushQuadrupel q (TEnv code table reg u) = TEnv (q:code) table reg u

tenvIncReg :: TEnv -> TEnv
tenvIncReg (TEnv c t reg u) = (TEnv c t (reg + 1) u)

tenvResetCode :: TEnv -> TEnv
tenvResetCode (TEnv code t r u) = TEnv [] t r u

tenvSetSymTable :: SymbolTable -> TEnv -> TEnv
tenvSetSymTable symt (TEnv c _ r u) = TEnv c symt r u

tenvInsertSymbol :: Name -> SymEntry -> TEnv -> TEnv
tenvInsertSymbol key sym (TEnv c symt r u) =
    let newSymT = defineSym symt key sym in (TEnv c newSymT r u)

prettyPrintQU :: QUnit -> IO ()
prettyPrintQU (QUnit meta fns) = do
    mapM_ ppF fns
  where
    ppF func = do
        putStrLn $ (qfunctionLabel func) ++ ":"
        mapM_ ppB (qfunctionPatterns func)
    ppB pattern = do
        putStrLn $ (qpatternLabel pattern) ++ ":"
        ppCs $ reverse (qpatternCode pattern)
    ppCs [] = return ()
    ppCs (c:cs) = do
        putStrLn $ "\t" ++ (show c)
        ppCs cs
