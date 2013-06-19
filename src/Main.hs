

import System.Environment
import System.Directory
import System.IO
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO
import qualified Data.Map as M
import qualified Data.List as DL

import Control.Monad

import Parser
import ParserTypes

import Interpretor
import Environment
import Builtins

main = do
    arguments <- getArgs
    handle arguments

handle :: [String] -> IO ()
handle ("-i":_) = repl newSymT
handle (f:_) = parse f
handle [] = putStrLn "usage: am [-i] [<file.am>]"

parse :: String -> IO ()
parse filePath = do
        result <- parseFile filePath
        case result of
            Left err -> print err
            Right prog -> do
                interpret prog

interpret :: Program -> IO ()
interpret prog = do
    syms <- return $ harvestSymbols (programFunctions prog) (SymbolTable M.empty)
    mMain <- return $ mainExpr syms
    case mMain of
        Just e -> do
            --putStrLn $ "interpret " ++ (show e)
            out <- return $ eval syms e 
            case out of
                Right out -> do 
                    putStrLn $ "evaluated " ++ (show out)
                Left msg -> do
                    putStrLn $ show msg
        Nothing -> putStrLn "could not find main!"

repl :: Env -> IO ()
repl env = do
    putStr ") "
    hFlush stdout
    line <- getLine
    if DL.isInfixOf "fn " line
      then replAddFunc env [line]
      else 
        if DL.isInfixOf "import " line
        then importFile env line
        else apply env line

importFile :: Env -> String -> IO ()
importFile env importstmt =
    let fileName = (unwords . (drop 1) . words) importstmt
      in do
        exists <- doesFileExist fileName
        if exists
          then do
            mProgram <- parseFile fileName
            case mProgram of
                Left err -> do 
                    print err
                    repl env
                Right program -> do
                    syms <- return $ harvestSymbols (programFunctions program) env
                    putStrLn $ "imported file '" ++ fileName ++ "'"
                    repl syms
          else do
            putStrLn $ "file '" ++ fileName ++ "' does not exist"
            repl env

apply :: Env -> String -> IO ()
apply env line = do
    result <- parseExprFromStr line
    case result of
        Left err -> do
            print err
            repl env
        Right expr -> do
            out <- return $ eval env expr
            case out of
                Right out -> do 
                    putStrLn $ "=> " ++ (show out)
                    repl env
                Left msg -> do
                    putStrLn $ show msg
                    repl env

replAddFunc :: Env -> [String] -> IO ()
replAddFunc env lines = do
    putStr "+ "
    hFlush stdout
    line <- getLine
    if length line == 0
      then do
        result <- parseFuncFromStr (unwords lines)
        case result of
            Left err2 -> do
                putStrLn "neither an expression nor a function given!"
                repl env
            Right func -> repl (defineSym env (funcName func) (SymFunc func))
      else
        replAddFunc env (lines ++ [line])


    
