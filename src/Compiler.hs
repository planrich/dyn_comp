module Compiler
    ( compile
    )
  where

import ParserTypes
import IR.Quadrupel as QC
import Environment

import Control.Monad.State
import Control.Monad.Trans.Error

compile :: Unit -> IO ()
compile p = do
    eResult <- runStateT (runErrorT $ QC.transformUnit p)
    
    --eResult <- QC.transform (firstExpr p) (symTable p)
    case eResult of
        Left err -> putStrLn (show err)
        Right qc -> do
            putStrLn $ show $ qc
  where
    symTable p = harvestSymbols (unitFunctions p) newSymT
    firstExpr p = patternExpr (functionPatterns ((unitFunctions p) !! 0) !! 0)


-- XXX compile a unit and save the representation into a platform independant format
--compileUnit :: Unit -> IO ()



