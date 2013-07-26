module Neart.Types
    ( Unit (..)
    , Function (..)
    , Pattern (..)
    , Binding (..)
    , MetaUnit (..)
    , functionArgumentCount
    , functionParameterCount
    , patternArgumentCount
    )
  where

type Export = String
type Import = String
type Name = String

-- | a unit is the internal representation of a syntactically valid file
data Unit t = Unit 
    { unitMeta :: MetaUnit
    , unitFunctions :: [Function t]
    , unitImports :: [Import]
--    , unitFunctionMap :: M.Map Name Function
    }
  deriving (Show)

-- | a function
data Function t = Function 
    { functionName :: Name
    , functionPatterns :: [Pattern t]
    }
  deriving (Show)

data MetaUnit = MetaUnit 
    { unitName :: String
    , unitMajorVersion :: Int
    , unitMinorVersion :: Int
    , unitPatchVersion :: Int
    , unitExports :: [Export]
    }
  deriving (Show)

data Pattern t = Pattern 
    { patternBindings :: [Binding]
    , patternCode :: t
    }
  deriving (Show)

data Binding = BNumber !Int
             | BString !String
             | BBool !Bool
             | BAnon
             | BVar !String
             | BList !(Binding,Binding)
             | BNil
             deriving (Show)

-- |How many arguments must a specific function get to be executed?
--  It is assumed that every pattern of a function has the
--  same amount of bindings.
functionArgumentCount :: Function t -> Int
functionArgumentCount (Function _ []) = error "function must have a pattern"
functionArgumentCount (Function _ (p:_)) = patternArgumentCount p

patternArgumentCount :: Pattern t -> Int
patternArgumentCount (Pattern bindings _) = length bindings

functionParameterCount :: Function t -> Int
functionParameterCount function = length $ functionPatterns function
