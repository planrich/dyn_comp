module Compiler
    ( compile
    )
  where

import Parser
import ParserTypes

import Interpretor
import Environment
import Builtins

import IR.QuadrupelTypes

import Control.Monad.Writer
import Control.Monad.State

import qualified Data.Map as M

data LookupTable a = LookupTable a

type FuncGlobEnv = LookupTable (M.Map String Func)
type ModuleEnv = LookupTable (M.Map String (M.Map String Func))

data CEnv = CEnv { compilationFuncGlobalEnv :: FuncGlobEnv
                 , compilationModuleEnv :: ModuleEnv
                 }

type CompilationEnv a = StateT CEnv IO a

data QuadrupelCode = QuadrupelCode { qrcCode :: [Quadrupel] }

type QCode = [Quadrupel]

type QGen m a = StateT QCode (CompilationEnv a) a
type QGeneration a = QGen (CompilationEnv a) a

type FuncName = String

harvestExportedFunctions :: Program -> CompilationEnv
harvestExportedFunctions program = return ()

lookupFunction :: FuncName -> QGeneration Maybe Func
lookupFunction fname = undefined

--- NEXT UP: builtin +,-,*,/,%, bool funcs, ... ,. .. ,.... -> custom functions


compile :: [String] -> IO ()
compile (filePath:_) = do
        result <- parseFile filePath
        case result of
            Left err -> print err
            Right prog -> do
                putStrLn (show prog)
                emptyEnv <- return $ CEnv (LookupTable M.empty) (LookupTable M.empty)
                (_, env) <- runStateT compileWithEnv emptyEnv
                putStrLn $ show $ map (simpleTransform) (programFunctions prog)
                --qrcode <- return $ QR.transform prog
                --putStrLn "Quadrupel:"
                --putStrLn (show qrcode)
                --AS.generate prog ""

transformToQuadrupel :: FuncName -> Expr -> QGeneration ()
transformToQuadrupel fname (AppExpr2 (func:params))
    mFunc <- lookupFunction func
  where
    paramCount = length params


simpleTransform :: Func -> QR
simpleTransform (Func name _ []) = QRFunc name []
simpleTransform (Func name _ (p:_)) = 
    let (_,qs) = runState (simpleTransformExpr (patternExpr p)) (QuadrupelCode []) in QRFunc name (qrcCode qs)

simpleTransformExpr :: Expr -> QCGen
simpleTransformExpr (AppExpr2 (fun:params)) = 
    undefined
  where
    paramCount = length params
simpleTransformExpr _ = do
    modify (\x -> QuadrupelCode $ (qrcCode x) ++ [QRAssign (Target "test") (Value 1)])



