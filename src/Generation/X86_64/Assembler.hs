
module Generation.X86_64.Assembler
    ( assembleToFile
    , newAssembleState
    )
  where


import Control.Monad.State
import Control.Monad.Trans.Error

import IR.Quadrupel.Types

import System.IO

import qualified Data.Map as M

type Indent = Bool
type MAssemble s a = ErrorT String (StateT s IO) a

data AssembleState = AssembleState { asHandle :: Handle 
                                   }

type Register = String
type LifeRange = (Int,Int)
type RealRegister = Int
type VirtRegister = Int

data RegisterState = Free
                   | Used

data RegisterAllocator = RegisterAllocator { raRegisters :: M.Map RealRegister RegisterState
                                           , raLifeRanges :: M.Map VirtRegister LifeRange
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
    simpleHeader
    mapM_ assembleFunction (qunitFunctions q)

assembleFunction :: QFunction -> MAssemble AssembleState ()
assembleFunction func = do
    writeToHandle False ""
    writeToHandle False ((qfunctionLabel func) ++ ":")
    mapM_ assembleBlock (qfunctionBlocks func)


assembleBlock :: QBlock -> MAssemble AssembleState ()
assembleBlock block = do
    mapM_ assembleQuadrupel (qblockCode block)

assembleQuadrupel :: Quadrupel -> MAssemble AssembleState ()
assembleQuadrupel q = do
    mapM_ (writeToHandle True) (toasm q)


toasm :: Quadrupel -> [String]
toasm (QReturn (Register r)) = ("movq %" ++ (show r) ++ ", %rax") : toasm (QReturn Nil)
toasm (QReturn (Constant o)) = ("movq $" ++ (show o) ++ ", %rax") : toasm (QReturn Nil)
toasm (QReturn Nil) = ["retq"]
toasm (QCall label) = ["callq " ++ label]
toasm _ = []

simpleHeader :: MAssemble AssembleState ()
simpleHeader = do
    writeToHandle True ".text"
    writeToHandle False ".global test__main"
    writeToHandle True ".type test__main, @function"

