--
-- Environment.hs
-- Copyright (C) 2013 rich <planrichi@gmail.com>
--

module Environment 
    ( harvestSymbols
    , lazyLoadUnits
    , mainExpr
    , newSymT
    , defineSym
    , findEntry
    , findFunc
    , findSymsNameStartsWith
    , defineBindings
    , SymbolTable (..)
    , SymEntry (..)
    , Env (..)
    )
  where

import qualified Data.Map.Strict as M
import Data.Maybe

import ParserTypes

type Env = SymbolTable

lazyLoadUnits :: Unit -> SymbolTable
lazyLoadUnits unit =
    let imported = load (unitImports unit) (SymbolTable M.empty) in
      harvestSymbols (unitFunctions unit) imported
  where
    load [] symt = symt
    load (i:is) symt = load is symt

harvestSymbols :: [Function] -> SymbolTable -> SymbolTable
harvestSymbols [] s = s
harvestSymbols (f:fs) s@(SymbolTable m) = harvestSymbols fs (defineSym s (functionName f) (SymFunc f))

mainExpr :: SymbolTable -> Maybe Expr
mainExpr t@(SymbolTable m) = do
    findFunc t "main" >>= firstPat . functionPatterns
  where
    firstPat ((Pattern [] e):ps) = Just e
    firstPat _ = Nothing

data SymEntry = SymFunc Function
              | SymBinding Expr
              | SymArgument Int
              deriving (Show)

data SymbolTable = SymbolTable (M.Map String SymEntry)
                 deriving (Show)


newSymT :: SymbolTable 
newSymT = SymbolTable M.empty

defineSym :: SymbolTable -> Name -> SymEntry -> SymbolTable
defineSym (SymbolTable t) k s = SymbolTable $ M.insert k s t

findEntry :: SymbolTable -> Name -> Maybe SymEntry
findEntry sym@(SymbolTable t) k = M.lookup k t

findFunc :: SymbolTable -> Name -> Maybe Function
findFunc sym@(SymbolTable t) k = 
    case M.lookup k t of
        Just (SymFunc f) -> Just f
        _ -> Nothing

findSymsNameStartsWith :: Name -> SymbolTable -> [Function]
findSymsNameStartsWith l (SymbolTable t) =
    (map (unpack . snd)) . M.toList $ M.filterWithKey (startswith l) t  
  where
    startswith l = (\x y -> (take (length l) x) == l)
    unpack (SymFunc f) = f

defineBindings :: Env -> [Binding] -> [Expr] -> Env
defineBindings env (b:bs) (e:es) =
    let bN = bindingName b in
      if isJust bN 
        then defineBindings (defineSym env (fromJust bN) (SymBinding e))  bs es
        else defineBindings env bs es
defineBindings env _ _ = env


