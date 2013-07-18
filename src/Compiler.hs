module Compiler
    ( compile
    )
  where


import ParserTypes
import IR.Quadrupel.Code as QC
import Environment

import Control.Monad.State

import System.IO
import Generation.X86_64.Assembler
import Control.Monad.Trans.Error

compile :: Unit -> IO ()
compile unit = do
    (eResult, _) <- runStateT (runErrorT $ QC.transformUnit) (emptyTransEnv (symTable unit) unit)
    
    --eResult <- QC.transform (firstExpr p) (symTable p)
    case eResult of
        Left err -> putStrLn (show err)
        Right qc -> do
            putStrLn $ show $ qc
            putStrLn "writing assembler code"
            handle <- openFile "test.s" WriteMode
            (eResult, _) <- runStateT (runErrorT $ assembleToFile qc) (newAssembleState handle)
            hClose handle
  where
    symTable p = harvestSymbols (unitFunctions p) newSymT


