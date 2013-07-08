module IR.QuadrupelTypes
    ( Quadrupel (..)
    , Target (..)
    , Value (..)
    , Operation (..)
    , QR (..)
    )
  where

import ParserTypes
import qualified Data.Map as M

data Operation = Add
               | Sub
               | Mul
               | Div
             deriving (Show)


-- use a map for this (hack)
resolveOperand :: String -> Maybe Operation
resolveOperand "+" = Just Add
resolveOperand "-" = Just Sub
resolveOperand "*" = Just Mul
resolveOperand "/" = Just Div
resolveOperand _ = Nothing


data Target = Target String
          deriving (Show)
data Value = Value Integer
           | NilValue
          deriving (Show)

data QR = QRUnit { qrUnit :: Unit
                 , qrFunctions :: [QR] 
                 }
        | QRFunc { qrFuncName :: String
                 , quadrupels :: [Quadrupel] 
                 }
      deriving (Show)

data Quadrupel = QRAssign
                    { qassignDest :: Target
                    , qassignValue :: Value
                    }
               | QROpAssign
                    { opAssignOp :: Operation
                    , opAssignDest :: Target
                    , opAssignOperand1 :: Value
                    , opAssignOperand2 :: Value
                    }
             deriving (Show)
