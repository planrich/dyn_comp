module Main
    ( main
    )
  where

import Parser
import ParserTypes
import Interpretor

import qualified Data.Map as M

data Test = Arithmetic Int Expr

data TestResult = TestResult Bool Test String String

main :: IO ()
main = do
    putStr "arithmetic suit"
    suits <- return $ [ Arithmetic 1 (LitExpr 6) 
                      , Arithmetic 2 (LitExpr 7)
                      ] 
    results <- mapM runTest suits
    putStrLn ""

    mapM_ inspect results
    failed' <- return $ failed results 0 
    ran <- return $ length results
    putStrLn ""
    putStrLn $ "ran " ++ (show ran) ++ " tests. failed: " ++ (show failed')

runTest :: Test -> IO TestResult
runTest test@(Arithmetic i expect) = do
    filePath <- return $ "test/arithmetic/" ++ (show i) ++ ".test"
    result <- parseFile filePath
    case result of
        Left err -> do
            putStr "P"
            return $ TestResult False test filePath (show err)
        Right prog -> do
            syms <- return $ harvestSymbols (programFunctions prog) (SymbolTable M.empty)
            mMain <- return $ mainExpr syms
            case mMain of
                Just e -> do
                    out <- return $ eval e
                    if out == expect
                      then return $ TestResult True test filePath ""
                      else return $ TestResult False test filePath ("expected: " ++ (show expect) ++ "\ngot: " ++ (show out))
                Nothing -> return $ TestResult False test filePath "could not find main"


inspect :: TestResult -> IO ()
inspect (TestResult False test filePath msg) = do
    putStrLn $ "failed: " ++ filePath
    putStrLn $ "> " ++ msg
    putStrLn ""
inspect _ = return () -- this is a valid test...

failed ::  [TestResult] -> Int -> Int
failed [] c = c
failed (t@(TestResult False _ _ _):ts) c = failed ts (c+1)
failed (t:ts) c = failed ts c


