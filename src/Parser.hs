module Parser
    ( file
    )
  where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as P
import Text.Parsec.Pos

import Control.Monad

import TokenDef
import ParserTypes

lexer :: P.TokenParser ()
lexer = P.makeTokenParser tokenDef

whiteSpace = P.whiteSpace lexer
lexeme = P.lexeme lexer
symbol = P.symbol lexer
natural = P.natural lexer
parens = P.parens lexer
semi = P.semi lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
squares = P.squares lexer

file :: Parser Program
file = do
    whiteSpace
    fs <- many function
    eof
    return $ Program fs

function :: Parser Func
function = do
    reserved "fn"
    fname <- identifier
    symbol ":"
    sig <- signature
    patterns <- many pattern
    return $ Func fname sig patterns

pattern :: Parser Pattern
pattern = do
    symbol "="
    subpatterns <- many binding
    symbol ";"
    expr' <- expr
    return $ Pattern subpatterns expr'

expr :: Parser Expr
expr = choice
    [ funExpr
    ]
    <?> "expression"

funExpr :: Parser Expr
funExpr = do
    atoms <- many1 atomExpr
    return $ AppExpr atoms

atomExpr :: Parser Expr
atomExpr = choice
    [ litExpr
    , varExpr
    , (parens $ expr)
    ]

litExpr :: Parser Expr
litExpr = do
    i <- natural
    return $ LitExpr i

varExpr :: Parser Expr
varExpr = do
    id <- identifier
    --var' <- var
    return $ VarExpr id

binding :: Parser Binding
binding = banonym <|> bvar <|> bnumber

bvar :: Parser Binding
bvar = do
    id <- identifier
    return $ BVar id

bnumber :: Parser Binding
bnumber = do
    n <- natural
    return $ BNumber (fromIntegral n)

banonym :: Parser Binding
banonym = do
    s <- symbol "_"
    return $ BAnon

signature :: Parser [Type]
signature = do
    sig <- sepBy typ (symbol "->")
    return sig

typ :: Parser Type
typ = do
          symbol "int"
          return TInt
      <|> do
          symbol "string"
          return TString
      <|> do
          typ' <- squares typ
          return $ TList typ'
