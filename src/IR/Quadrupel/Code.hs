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


transformExpr :: Expr -> QCT Operand 
transformExpr (LitExpr i) = return $ Constant $ fromIntegral i
transformExpr expr@(Expr ((VarExpr fname):es)) = do
    tenv <- get
    mSym <- return $ findSymbol tenv fname
    case mSym of
        Just sym -> apply sym es
        Nothing -> throwError $
          IR.Quadrupel.Types.SymbolNotFound ("could not find symbol '" ++ fname ++ "'") expr

transformBinding :: Label -> Index -> Binding -> QCT Index
transformBinding nextLabel index BAnon = return $ index + 1
transformBinding nextLabel index (BVar name) = do
    modify (\tenv -> tenvInsertSymbol name (SymArgument index) tenv)
    return $ index + 1

transformPattern :: Label -> Label -> Pattern -> QCT QBlock
transformPattern nextLabel label pattern = do
    tenv <- get

    -- save the old symbol table. trasnformBinding is going to add new symbols
    oldSymT <- return $ tenvSymTable tenv

    foldM_ (transformBinding nextLabel) 0 (patternBindings pattern)
    operand <- transformExpr (patternExpr pattern)

    qc <- return $ tenvCode tenv
    put $ tenvSetSymTable oldSymT (tenvResetCode tenv)

    return $ QBlock label qc

patternFallThroughLabel = "__pattern_fall_through"

type Label = String
type LabelPrefix = String
type Index = Int

transformPatterns :: LabelPrefix -> Int -> [Pattern] -> QCT [QBlock]
transformPatterns labelPrefix index [] = return $ []
transformPatterns labelPrefix index (p:ps) = do
    nextLabel <- return $ if length ps >= 1 then patternLabel labelPrefix (index+1) else patternFallThroughLabel
    np <- transformPattern nextLabel (patternLabel labelPrefix index) p
    nps <- transformPatterns labelPrefix (index+1) ps
    return (np:nps)
  where
    patternLabel prefix index = prefix ++ "__" ++ (show index)

transformFunction :: LabelPrefix -> Function -> QCT QFunction
transformFunction labelPrefix func = do
    qBlocks <- transformPatterns newLabelPrefix 0 (functionPatterns func)
    return $ QFunction (functionName func) newLabelPrefix qBlocks
  where
    newLabelPrefix = labelPrefix ++ "__" ++ (functionName func)


transformUnit :: QCT QUnit
transformUnit = do
    modify (\tenv -> let newSymT = (lazyLoadUnits (tenvUnit tenv)) in tenvSetSymTable newSymT tenv)
    tenv <- get

    let unit = tenvUnit tenv
        uName = unitName $ unitMeta $ tenvUnit tenv in
      mapM (\f -> transformFunction uName f) (unitFunctions $ unit) >>= (\funcs -> return $ QUnit (unitMeta unit) funcs)

findSymbol :: TEnv -> Name -> Maybe Symbol
findSymbol tenv name = do
    mplus (liftM (\_ -> FuncSym (uName ++ "__" ++ name)) (findFunc (tenvSymTable tenv) name)) 
          (liftM (\op -> CoreFunc op) (M.lookup name builtinMap))
  where
    uName = unitName $ unitMeta $ tenvUnit tenv

apply :: Symbol -> [Expr] -> QCT Operand
apply (FuncSym qualifiedName) params = do
    pushInstr $ QCall qualifiedName
    return $ Constant 1
apply (CoreFunc op) params = do
    results <- mapM transformExpr params
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
            pushInstr $ QAssignOp reg op op1 op2
            return $ Register reg
        Just operand -> return $ operand

prettyPrint :: Quadrupel -> IO ()
prettyPrint (QAssignOp r (Binary op) op1 op2) = 
    putStrLn $ (show (Register r)) ++ " = " ++ (show op1) ++ " " ++ (show op) ++ " " ++ (show op2)


