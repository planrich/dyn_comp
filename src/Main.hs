

import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO
import qualified Data.Map as M

import Control.Monad

import Parser
import ParserTypes

import Interpretor
import Environment
import Builtins

main = do
    arguments <- getArgs
    when (length arguments > 0) $ do
        parse (arguments !! 0)

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

