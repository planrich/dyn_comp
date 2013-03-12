
import Text.Parsec.String
import Text.Parsec.Prim
import Text.Parsec.Error

import System.Environment
import System.IO
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as LIO

import Control.Monad

import Parser

main = do
    arguments <- getArgs
    when (length arguments > 0) $ do
        result <- parseFromFile file (arguments !! 0)
        case result of
            Left err -> print err
            Right xs -> do
                putStrLn $ "parsing succeeded ast:"
                putStrLn $ (show xs)

{-
parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname = do
    input <- readFile fname
    return $ runP p () fname input

-}
