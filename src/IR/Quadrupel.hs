module IR.Quadrupel
    ( Operation (..)
    , Quadrupel (..)
    , QuadrupelCode (..)
    , transform
    , transformUnit
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


builtinMap = M.fromList 
             [ ("add", Binary Add)
             , ("mul", Binary Mul)
             , ("div", Binary Div)
             , ("sub", Binary Sub)
             ]

isConstant :: Operand -> Bool
isConstant (Constant i) = True
isConstant _ = False

arithOptimize :: Operation -> Operand -> Operand -> Maybe Operand
arithOptimize (Binary op) op1 op2 = runArithBinOpt op op1 op2
-- Cannot optimize
arithOptimize _ _ _ = Nothing

runArithBinOpt :: BinOp -> Operand -> Operand -> Maybe Operand
runArithBinOpt Add (Constant i1) (Constant i2) = Just $ Constant ((fromIntegral i1) + (fromIntegral i2))
runArithBinOpt Sub (Constant i1) (Constant i2) = Just $ Constant ((fromIntegral i1) - (fromIntegral i2))
-- it is possible to catch a div by zero here
-- here a floating point should result
--runArithBinOpt Div (Constant i1) (Constant i2) = Just $ Constant $ toInteger ((fromIntegral i1) / (fromIntegral i2))
runArithBinOpt Mul (Constant i1) (Constant i2) = Just $ Constant ((fromIntegral i1) * (fromIntegral i2))
runArithBinOpt _ _ _ = Nothing

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

type GQCT e s a = ErrorT e (StateT s IO) a
type QCT a = GQCT TransformError TEnv a

type Module = String

data Symbol = FuncSym Module Name
            | CoreFunc Operation
    deriving (Show)

failTransform :: TransformError -> QCT Operand
failTransform e = throwError e

quadrupelize :: Expr -> QCT Operand
quadrupelize (LitExpr i) = return $ Constant $ fromIntegral i
quadrupelize expr@(Expr ((VarExpr fname):es)) = do
    tenv <- get
    mSym <- return $ findSymbol tenv fname
    case mSym of
        Just sym -> apply sym es
        Nothing -> failTransform $ IR.Quadrupel.SymbolNotFound ("could not find symbol '" ++ fname ++ "'") expr

-- Unit quadrupel transformation context
type UQT a = ErrorT TransformError (StateT Unit IO) a

transformExpr :: Expr -> SymbolTable -> UQT QuadrupelCode
transformExpr expr symTable = do
    (error, tenv) <- runStateT (runErrorT $ quadrupelize $ expr) (TEnv [] symTable 0)
    case error of
        Left error -> return $ Left error
        Right operand -> return $ Right $ QCode (code tenv) operand

transformFunction :: Function -> SymbolTable -> UQT QFunction
transformFunction func symt = do
    
    --qCodes <- mapM (\pattern -> transformPattern (patternExpr pattern) symt) (functionPatterns func)
    throwError $ DefaultError "fail"

transformUnit :: UQT QUnit
transformUnit unit = do
    unit <- get
    symt <- return $ lazyLoadUnits unit
    
    mapM (\f -> transformFunction f symt) (unitFunctions unit) >>= (\funcs -> return $ QUnit (unitMeta unit) funcs)

findSymbol :: TEnv -> Name -> Maybe Symbol
findSymbol tenv name = do
    mplus (liftM (\_ -> FuncSym "" name) (findFunc (symTable tenv) name)) 
          (liftM (\op -> CoreFunc op) (M.lookup name builtinMap))

apply :: Symbol -> [Expr] -> QCT Operand
apply (FuncSym _ _) params = return $ Constant 1
apply (CoreFunc op) params = do
    results <- mapM quadrupelize params
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
    -- try to optimize this function
    mOperand <- return $ arithOptimize op op1 op2
    case mOperand of
        Nothing -> do
            -- could not optimize
            reg <- uniqueReg
            pushInstr $ Quadrupel reg op op1 op2
            return $ Register reg
        Just operand -> return $ operand

prettyPrint :: Quadrupel -> IO ()
prettyPrint (Quadrupel r (Binary op) op1 op2) = 
    putStrLn $ (show (Register r)) ++ " = " ++ (show op1) ++ " " ++ (show op) ++ " " ++ (show op2)


