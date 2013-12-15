module Neart.Types
    ( Unit (..)
    , Function (..)
    , MetaUnit (..)
    )
  where

type Export = String
type Import = String
type Name = String

data MetaUnit = MetaUnit 
    { unitName :: String
    , unitMajorVersion :: Int
    , unitMinorVersion :: Int
    , unitPatchVersion :: Int
    , unitExports :: [Export]
    }
  deriving (Show)

class CompUnit u where
    unitFunctions :: CompFunction f => u -> [f]
    unitTransform :: (CompFunction f1, CompFunction f2) => (f1 -> f2) -> u -> u

class CompFunction f where
    funcName :: f -> Name
    funcCode :: CompCode c => f -> c
    funcTransform :: (CompCode c1, CompCode c2) => (c1 -> c2) -> f -> f
    funcArgCount :: f -> Int

class CompCode c where
    codeArgCount :: c -> Int
    --where
    --codeMeta :: CMeta m => p -> m

-- | a unit is the internal representation of a syntactically valid file
data (CompFunction f) => Unit f = Unit MetaUnit [f] deriving (Show)

instance (CompFunction f) => CompUnit (Unit f) where
    unitFunctions (Unit m fs) = fs
    unitTransform func (Unit m fs) = (Unit m (map func fs))

data (CompCode c) => Function c = Function Name c deriving (Show)
instance CompFunction (Function c) where
    funcName (Function n _) = n
    funcCode (Function _ p) = p
    funcTransform f (Function n p) = (Function n (f p))

    funcArgCount (Function _ _) = 0 -- error "fatal: function does not have a pattern which is invalid"
--    funcArgCount (Function _ []) = error "fatal: function does not have a pattern which is invalid"
--    funcArgCount (Function _ (p:_)) = length $ patternBindings p

transformCompCode :: (CompUnit c1, CompUnit c2) => (a -> b) -> c1 -> c2
transformCompCode f unit = unitTransform (\u -> map convertFunction (unitFunctions u)) unit
  where
    convertFunction funcs = funcTransform (\func -> map f (funcCode func)) funcs


