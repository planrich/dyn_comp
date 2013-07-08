module AssemblerTarget
    ( generate
    )
  where

import ParserTypes

import Interpretor
import Environment
import Builtins

import Control.Monad.Writer

generate :: Program -> String -> IO ()
generate (Program unit functions) file = do
    (_,asm) <- return $ runWriter (generateAsm unit functions)
    putStrLn (show asm)

generateAsm :: Unit -> [Func] -> Writer [String] ()
generateAsm _ [] = undefined
generateAsm u (f:fs) = do
    tell [asmname]
  where
    fname = funcName f
    uname = unitName u
    asmname = uname ++ "_" ++ fname ++ ":"


