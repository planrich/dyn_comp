

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

main = do
    arguments <- getArgs
    when (length arguments > 0) $ do
        result <- parseFile (arguments !! 0)
        case result of
            Left err -> print err
            Right prog -> do
                putStrLn $ "parsing succeeded ast:"
                putStrLn $ (show prog)
                putStrLn $ "evaluating main:"
                syms <- return $ harvestSymbols (programFunctions prog) (SymbolTable M.empty)
                mMain <- return $ mainExpr syms
                case mMain of
                    Just e -> do
                        putStrLn $ "doing " ++ (show e)
                        out <- return $ eval e 
                        putStrLn $ "evaluated " ++ (show out)
                    Nothing -> putStrLn "could not find main!"


{-
parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname = do
    input <- readFile fname
    return $ runP p () fname input

-}
