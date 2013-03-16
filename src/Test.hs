module Main
    ( main
    )
  where

import Parser
import ParserTypes
import Interpretor

import qualified Data.Map as M

type Test = (Int,TestType)

data TestType = EvalTo Expr
              | ThrowTypeMissmatch

data TestResult = TestResult Bool TestType String String

main :: IO ()
main = do
    putStr "running test suite"
    suits <- return $ [ (1, EvalTo (LitExpr 6))
                      , (2, EvalTo (LitExpr 7))
                      , (3, EvalTo (LitExpr 20))
                      , (4, EvalTo (BoolExpr True))
                      , (5, EvalTo (BoolExpr False))
                      , (6, ThrowTypeMissmatch)
                      ] 
    results <- mapM runTest suits
    putStrLn ""

    mapM_ inspect results
    failed' <- return $ failed results 0 
    ran <- return $ length results
    putStrLn ""
    putStrLn $ "ran " ++ (show ran) ++ " tests. failed: " ++ (show failed')

runTest :: Test -> IO TestResult
runTest (i, test) = do
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
                    checkExpr out test filePath
                _ -> return $ TestResult False test filePath "could not find main"

checkExpr :: ThrowError Expr -> TestType -> String -> IO TestResult
checkExpr out test@(EvalTo expected) filePath =
    case out of
        Right expr -> do
            case expr == expected of
                True -> do
                    putStr "."
                    return $ TestResult True test filePath ""
                _ -> do
                    putStr "E"
                    return $ TestResult False test filePath ("expected: " ++ (show expected) ++ "\ngot: " ++ (show expr))
        Left msg -> do
            putStr "F"
            return $ TestResult False test filePath ("evaluation failed: " ++ (show msg))
checkExpr out test@(ThrowTypeMissmatch) filePath =
    case out of
        Left (TypeMissmatch _ _) -> do
            putStr "."
            return $ TestResult True test filePath ""
        _ -> do
            putStr "F"
            return $ TestResult False test filePath "no type missmatch, but was expected"


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


