module Parser
    ( parseFile
    )
  where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Pos
import Text.Parsec.Combinator
import Text.Parsec.Error
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P

import Control.Monad

import TokenDef
import ParserTypes

parseFile :: String -> IO (Either ParseError Program)
parseFile filePath = parseFromFile file filePath

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
    [ lamExpr
    , condExpr
    , funExpr
    ]
    <?> "expression"

lamExpr :: Parser Expr
lamExpr = do
    reservedOp "\\"
    id <- identifier
    reservedOp "->"
    body' <- expr
    return $ LamExpr id body' 

condExpr :: Parser Expr
condExpr = do
    reserved "if"
    cond <- expr
    reserved "then"
    alt1 <- expr
    reserved "else"
    alt2 <- expr
    return $ CondExpr cond alt1 alt2

funExpr :: Parser Expr
funExpr = do 
    atoms <- many atomExpr
    return $ foldl1 AppExpr atoms

atomExpr :: Parser Expr
atomExpr = try $ choice
    [ litExpr
    , boolExpr
    , varExpr
    , listExpr
    , (parens $ expr)
    ]

nilExpr :: Parser Expr
nilExpr = do
    symbol "["
    symbol "]"
    return $ ListExpr []
    
listExpr :: Parser Expr
listExpr = 
    (try nilExpr)
    <|> do
        elems <- between (reservedOp "[") (reservedOp "]") (sepBy expr (reservedOp ","))
        return $ ListExpr elems

boolExpr :: Parser Expr
boolExpr = do{ reserved "true"; return $ BoolExpr True}
    <|> do { reserved "false"; return $ BoolExpr False}

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
binding = banonym <|> bvar <|> bnumber <|> bbool

bbool :: Parser Binding
bbool = 
    do
      reserved "true"
      return $ BBool True
    <|> do
      reserved "false"
      return $ BBool False

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

