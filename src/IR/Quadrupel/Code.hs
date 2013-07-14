module IR.Quadrupel.Code
    ( Operation (..)
    , Quadrupel (..)
    , QuadrupelCode (..)
    , transformExpr
    , transformUnit
    , prettyPrint
    , emptyTransEnv
    )
  where

import IR.Quadrupel.Types

import ParserTypes
import Environment
import Builtins

import Control.Monad.State
import Control.Monad.Trans.Error

import qualified Data.Map as M

type GQCT e s a = ErrorT e (StateT s IO) a
type QCT a = GQCT TransformError TEnv a

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


quadrupelize :: Expr -> QCT Operand
quadrupelize (LitExpr i) = return $ Constant $ fromIntegral i
quadrupelize expr@(Expr ((VarExpr fname):es)) = do
    tenv <- get
    mSym <- return $ findSymbol tenv fname
    case mSym of
        Just sym -> apply sym es
        Nothing -> throwError $
          IR.Quadrupel.Types.SymbolNotFound ("could not find symbol '" ++ fname ++ "'") expr

transformExpr :: Expr -> QCT QuadrupelCode
transformExpr expr = do
    quadrupelize expr >>= (\operand -> do
        tenv <- get
        qcode <- return $ QCode (code tenv) operand
        put $ tenvResetCode tenv
        return qcode
        )

transformFunction :: Function -> QCT QFunction
transformFunction func = do
    
    --qCodes <- mapM (\pattern -> transformPattern (patternExpr pattern) symt) (functionPatterns func)
    throwError $ DefaultError "fail"

transformUnit :: QCT QUnit
transformUnit = do
    modify (\tenv -> let newSymT = (lazyLoadUnits (tenvUnit tenv)) in tenvSetSymTable newSymT tenv)
    tenv <- get

    let unit = tenvUnit tenv in
      mapM (\f -> transformFunction f) (unitFunctions $ unit) >>= (\funcs -> return $ QUnit (unitMeta unit) funcs)

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
pushInstr q = modify (tenvPushQuadrupel q)

uniqueReg :: QCT Reg
uniqueReg = do
    tenv <- get
    put (tenvIncReg tenv)
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


