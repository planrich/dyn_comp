module IR.Quadrupel.Optimizer
    ( arithOptimize
    )
  where

import IR.Quadrupel.Types

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

