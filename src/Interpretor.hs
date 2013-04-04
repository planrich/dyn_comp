module Interpretor 
    ( eval
    , SymbolTable (..)
    , harvestSymbols
    , mainExpr
    )
  where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Error
import Data.Maybe

import ParserTypes

type Env = SymbolTable

eval :: Env -> Expr -> ThrowError Expr
eval env (AppExpr f s) = apply env f s
eval env (ListExpr es) = liftM ListExpr $ mapM (eval env) es
eval env (VarExpr n) = do
    mEntry <- return $ findEntry env n
    case mEntry of
        (Just (SymBinding expr)) -> eval env expr
        Nothing -> throwError $ SymbolNotFound (n ++ (show env))
eval env e
    | isAtom e = return $ e -- cannot reduce further
    | otherwise =  throwError $ Fallback (show e)

apply :: Env -> Expr -> Expr -> ThrowError Expr
-- ^ Nested application recurse
apply env a@(AppExpr _ _) args = (eval env a) >>= (\a -> apply env a args)
-- TODO
apply env c@(CurryExpr f e1) (VarExpr n) =
    case M.lookup n builtins of
        Just b -> return $ CurryExpr b c
        Nothing -> do
            mEntry <- return $ findEntry env n
            case mEntry of
                (Just (SymBinding expr)) -> (eval env expr) >>= (\a -> apply env c a)
                Nothing -> throwError $ SymbolNotFound (n ++ (show env))
--apply env c1@(CurryExpr f1 e1) c2@(CurryExpr f2 e2) = undefined

-- ^Apply a function context to another arg. Basically there are two options:
-- The arguments suffice the function -> function is executed
-- There are too little arguemts -> context swallows argument and proceeds
apply env c@(FuncCtx f args) arg
    | (length args) + 1 == builtinParamCount f = f (args ++ [arg])
    | otherwise = return $ FuncCtx f (args ++ [arg])

-- ^At the bottom of a recursion an AppExpr might contain a Builtin or call any other
-- function that is defined in the loaded files
apply env (VarExpr n) args =
    case M.lookup n builtins of
        -- if it is a unary function handle it right away
        Just (Builtin 1 f) -> (eval env args) >>= (\args -> f [args])
        -- n-ary function must harvest additional n-1 parameters
        Just b -> (eval env args) >>= (\args -> return $ FuncCtx b [args])
        -- TODO 
        Nothing -> applyFromEnv env (VarExpr n) args
-- TODO -> better error
apply env func args = throwError $ SymbolNotFound $ (show env) ++ (show func) ++ (show args)

applyFromEnv :: Env -> Expr -> Expr -> ThrowError Expr
applyFromEnv env (VarExpr n) arg = do
    sym <- return $ findFunc env n
    pat <- return $ sym >>= ((match [arg]) . funcPatterns)
    case pat of
        Just (Pattern binding expr) -> eval (defineBindings env binding [arg]) expr
        _ -> case sym of
                Nothing -> throwError $ SymbolNotFound n
                _ -> throwError $ PatternFallthrough n [arg]

defineBindings :: Env -> [Binding] -> [Expr] -> Env
defineBindings env (b:bs) (e:es) =
    let bN = bindingName b in
      if isJust bN 
        then defineBindings (defineSym env (fromJust bN) (SymBinding e))  bs es
        else defineBindings env bs es
defineBindings env _ _ = env


applyCurry :: Expr -> Expr -> ThrowError Expr
applyCurry (CurryExpr f e) param
    | isCurry e = (applyCurry e param)
    | otherwise = -- f [e, param]
        case f of
            Builtin 2 f -> f [e,param]
            Builtin n f -> undefined

subs :: Expr -> Expr -> Expr -> Expr
subs a x e
    | isId e = if e == x then a else e
    | isApp e = AppExpr (subs a x (getFunction e)) (subs a x (getArgument e))
    | otherwise = let y = getId e in let c = getBody e in
        if y == x 
          then e 
          else let z = (VarExpr "u'") in LamExpr z (subs a x (subs z y c))

existsBuiltin :: Int -> String -> Maybe Builtin
existsBuiltin argcount name =
    case M.lookup name builtins of
        Just bi@(Builtin argcount' b) -> if argcount' == argcount then Just bi else Nothing
        _ -> Nothing

builtins :: M.Map String Builtin
builtins = M.fromList [ ("add", Builtin 2 $ numericBinary (+))
                      , ("+",   Builtin 2 $ numericBinary (+))
                      , ("sub", Builtin 2 $ numericBinary (-))
                      , ("-",   Builtin 2 $ numericBinary (-))
                      , ("div", Builtin 2 $ numericBinary div)
                      , ("/",   Builtin 2 $ numericBinary div)
                      , ("mul", Builtin 2 $ numericBinary (*))
                      , ("*",   Builtin 2 $ numericBinary (*))
                      , ("mod", Builtin 2 $ numericBinary mod)
                      , ("and", Builtin 2 $ boolBinary (&&))
                      , ("or",  Builtin 2 $ boolBinary (||))
                      , ("head", Builtin 1 $ extractListOp 1 (head))
                      --, ("tail", Builtin 1 extractListOp (tail))
                      ]

extractListOp :: Int -> ([Expr] -> Expr) -> [Expr] -> ThrowError Expr
extractListOp c f [(ListExpr ex)] = if length ex >= c then return $ f ex else throwError $ InvalidArgument "provide more arguments for that operation"
extractListOp _ _ (ex:_) = throwError $ TypeMissmatch "expected list" ex

boolBinary :: (Bool -> Bool -> Bool) -> [Expr] -> ThrowError Expr
boolBinary op [(BoolExpr a1), (BoolExpr a2)] = return $ BoolExpr $ op a1 a2
--boolBinary op [e1@(BoolExpr a1), e2] = (eval e2) >>= (\e2 -> boolBinary op [e1,e2])
--boolBinary op [e2,e1@(BoolExpr a1)] = (eval e1) >>= (\e1 -> boolBinary op [e1,e2])
boolBinary op (e1:_) = throwError $ TypeMissmatch "expected bool" e1 

numericBinary :: (Integer -> Integer -> Integer) -> [Expr] -> ThrowError Expr
numericBinary op [(LitExpr a1), (LitExpr a2)] = return $ LitExpr $ op a1 a2
--numericBinary op [e1@(LitExpr a1), e2@(AppExpr _ _)] = (eval e2) >>= (\e2 -> numericBinary op [e1, e2])
--numericBinary op [e1@(AppExpr _ _), e2@(LitExpr a1)] = (eval e1) >>= (\e1 -> numericBinary op [e1, e2])
numericBinary op (e1:_) = throwError $ TypeMissmatch "expected number" e1 

harvestSymbols :: [Func] -> SymbolTable -> SymbolTable
harvestSymbols [] s = s
harvestSymbols (f:fs) s@(SymbolTable m) = harvestSymbols fs (defineSym s (funcName f) (SymFunc f))

mainExpr :: SymbolTable -> Maybe Expr
mainExpr t@(SymbolTable m) = do
    findFunc t "main" >>= firstPat . funcPatterns
  where
    firstPat ((Pattern [] e):ps) = Just e
    firstPat _ = Nothing

data SymEntry = SymFunc Func
              | SymBinding Expr
              deriving (Show)

data SymbolTable = SymbolTable (M.Map String SymEntry)
                 deriving (Show)

newSymT :: SymbolTable 
newSymT = SymbolTable M.empty

defineSym :: SymbolTable -> Name -> SymEntry -> SymbolTable
defineSym (SymbolTable t) k s = SymbolTable $ M.insert k s t

findEntry :: SymbolTable -> Name -> Maybe SymEntry
findEntry sym@(SymbolTable t) k = M.lookup k t

findFunc :: SymbolTable -> Name -> Maybe Func
findFunc sym@(SymbolTable t) k = 
    case M.lookup k t of
        Just (SymFunc f) -> Just f
        _ -> Nothing

isAtom :: Expr -> Bool
isAtom (VarExpr _) = True
isAtom (BoolExpr _) = True
isAtom (LitExpr _) = True
isAtom (ListExpr _) = True
isAtom _ = False

getId :: Expr -> Expr
getId e@(VarExpr n) = e

getName :: Expr -> Name
getName (VarExpr n) = n
getName _ = ""

isId :: Expr -> Bool
isId (VarExpr _) = True
isId _ = False

isCurry :: Expr -> Bool
isCurry (CurryExpr _ _) = True
isCurry _ = False

isFunc :: Expr -> Bool
isFunc (LamExpr _ _) = True
isFunc _ = False

getFunction :: Expr -> Expr
getFunction (AppExpr e e1) = getFunction e
getFunction l@(LamExpr _ _) = l

getArgument :: Expr -> Expr
getArgument (AppExpr _ e) = e

getBody :: Expr -> Expr
getBody (LamExpr _ b) = b

isApp :: Expr -> Bool
isApp (AppExpr _ _) = True
isApp _ = False

