module Compiler
    ( compile
    )
  where

import ParserTypes
import IR.Quadrupel.Code as QC
import Environment

import Control.Monad.State
import Control.Monad.Trans.Error

compile :: Unit -> IO ()
compile unit = do
    (eResult, _) <- runStateT (runErrorT $ QC.transformUnit) (emptyTransEnv (symTable unit) unit)
    
    --eResult <- QC.transform (firstExpr p) (symTable p)
    case eResult of
        Left err -> putStrLn (show err)
        Right qc -> do
            putStrLn $ show $ qc
  where
    symTable p = harvestSymbols (unitFunctions p) newSymT


