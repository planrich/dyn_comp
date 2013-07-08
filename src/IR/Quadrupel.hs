module IR.Quadrupel
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




