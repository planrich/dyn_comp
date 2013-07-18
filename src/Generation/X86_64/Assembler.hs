
module Generation.X86_64.Assembler
    ( assembleToFile
    , newAssembleState
    )
  where


import Control.Monad.State
import Control.Monad.Trans.Error

import IR.Quadrupel.Types

import System.IO

type Indent = Bool
type MAssemble s a = ErrorT String (StateT s IO) a

data AssembleState = AssembleState { asHandle :: Handle 
                                   }

newAssembleState :: Handle -> AssembleState
newAssembleState h = AssembleState h

writeToHandle :: Indent -> String -> MAssemble AssembleState ()
writeToHandle b s
    | b = do
        handle <- liftM asHandle get
        lift $ lift $ hPutStr handle "    "
        writeToHandle False s
    | otherwise = do
        handle <- liftM asHandle get
        lift $ lift $  hPutStrLn handle s
      


assembleToFile :: QUnit -> MAssemble AssembleState ()
assembleToFile q = do
    mapM_ assembleFunction (qunitFunctions q)

assembleFunction :: QFunction -> MAssemble AssembleState ()
assembleFunction func = do
    writeToHandle False ""
    writeToHandle False ((qfunctionName func) ++ ":")
