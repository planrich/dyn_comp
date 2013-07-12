module Compiler
    ( compile
    )
  where

import ParserTypes
import IR.Quadrupel as QC
import Environment

import Control.Monad.State
import Control.Monad.Trans.Error

compile :: Program -> IO ()
compile p = do
    eResult <- QC.transform (firstExpr p) (symTable p)
    case eResult of
        Left err -> putStrLn (show err)
        Right qc -> do
            mapM_ (\x -> prettyPrint x) (qcode qc)
            putStrLn $ show $ qcodeResult qc
  where
    symTable p = harvestSymbols (programFunctions p) newSymT
    firstExpr p = patternExpr (funcPatterns ((programFunctions p) !! 0) !! 0)


-- XXX compile a unit and save the representation into a platform independant format
--compileUnit :: Unit -> IO ()



