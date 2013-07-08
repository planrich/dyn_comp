module Compiler
    ( compile
    )
  where

import ParserTypes
import IR.Quadrupel as QC
import Environment

import Control.Monad.State

compile :: Program -> IO ()
compile p = do
    qc <- execStateT (QC.transform $ firstExpr p) (TEnv [] (symTable p) 0)
    putStrLn $ show qc
  where
    symTable p = harvestSymbols (programFunctions p) newSymT
    firstExpr p = patternExpr (funcPatterns ((programFunctions p) !! 0) !! 0)
