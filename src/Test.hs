module Main
    ( main
    )
  where

import Parser
import ParserTypes
import Interpretor
import Environment
import Builtins
import System.IO
import System.Environment

import qualified Data.Map as M

data TestResult = TestResult Bool Name String

main :: IO ()
main = do
    args <- getArgs
    runSuits "test" args

testFuncs :: SymbolTable -> [(Name, Expr)]
testFuncs t = do
     map unpack (findSymsNameStartsWith "test" t)
  where
    firstPat ((Pattern [] e):ps) = e
    firstPat _ = undefined

    unpack func = (funcName func, ((firstPat . funcPatterns) func))

runSuits :: String -> [String] -> IO ()
runSuits _ [] = return ()
runSuits folder (t:ts) = do
    putStr $ "running test suite '" ++ t ++ "': "
    filePath <- return $ folder ++ "/" ++ t ++ ".test"
    result <- parseFile filePath
    case result of
        Left err -> do
            putStrLn "parse error!"
            putStrLn ""
            putStrLn (show err)
        Right prog -> do
            syms <- return $ harvestSymbols (programFunctions prog) (SymbolTable M.empty)
            tests <- return $ testFuncs syms
            results <- runTests tests syms
            putStrLn ""
            mapM_ (inspect t) results
            failed' <- return $ failed results 0 
            ran <- return $ length results
            putStrLn ""
            putStrLn $ "ran " ++ (show ran) ++ " tests. failed: " ++ (show failed')

    runSuits folder ts

runTests :: [(Name, Expr)] -> Env -> IO [TestResult]
runTests [] _ = return $ []
runTests ((n,e):es) env
    | isAssert e = do
       result <- return $ eval env (toEval e)
       case result of
         Left err -> do
           putStr "E"
           runTests es env >>= return . (:) (TestResult False n ("unexpected error: " ++ (show err)))
         Right computed -> do
           case computed == (goal e) of
             True -> do
               putStr "."
               runTests es env >>= return . (:) (TestResult True n "")
             False -> do
               putStr "E"
               runTests es env >>= return . (:) (TestResult False n ("expected: " ++ (show (goal e)) ++ "\ngot: " ++ (show computed)))
    | isAssertTypeMissmatch e = do
       result <- return $ eval env (sndAppExpr e)
       case result of
         Left err -> do
           putStr "."
           runTests es env >>= return . (:) (TestResult True n "")
         _ -> do
           putStr "E"
           runTests es env >>= return . (:) (TestResult False n "expected type missmatch")
    | otherwise = do
       putStrLn $ "could not match " ++ (show e)
       runTests es env

isAssertTypeMissmatch :: Expr -> Bool
isAssertTypeMissmatch (AppExpr (VarExpr "assert_type_missmatch") e) = True
isAssertTypeMissmatch _ = False

sndAppExpr :: Expr -> Expr
sndAppExpr (AppExpr _ e) = e
sndAppExpr _ = undefined

isAssert :: Expr -> Bool
isAssert (AppExpr (AppExpr (VarExpr "assert_eq") _) _) = True
isAssert _ = False

toEval :: Expr -> Expr
toEval (AppExpr (AppExpr (VarExpr _) e) _) = e
toEval _ = undefined

goal :: Expr -> Expr
goal (AppExpr (AppExpr (VarExpr _) _) e) = e
goal _ = undefined

inspect :: Name -> TestResult -> IO ()
inspect suit (TestResult False name msg) = do
    putStrLn ""
    putStrLn $ "(" ++ suit ++ ") failed: " ++ name
    putStrLn $ "> " ++ msg
inspect _ _ = return () -- this is a valid test...

failed ::  [TestResult] -> Int -> Int
failed [] c = c
failed (t@(TestResult False _ _):ts) c = failed ts (c+1)
failed (t:ts) c = failed ts c

