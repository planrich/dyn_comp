module Parser
    ( parseFile
    , parseExprFromStr
    , parseFuncFromStr
    )
  where

import Text.Parsec
import Text.Parsec.String as PS
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
parseFile filePath = PS.parseFromFile file filePath

parseExprFromStr :: String -> IO (Either ParseError Expr)
parseExprFromStr input = return (runP replExpr () "repl" input)

parseFuncFromStr :: String -> IO (Either ParseError Func)
parseFuncFromStr input = return (runP replFunction () "repl" input)

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
operator = P.operator lexer
reservedOp = P.reservedOp lexer
squares = P.squares lexer
stringLiteral = P.stringLiteral lexer

replExpr :: Parser Expr
replExpr = do
    e <- expr
    eof
    return e

replFunction :: Parser Func
replFunction = do
    f <- function
    eof
    return f

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
    case atoms of
        [] -> fail "not an expression"
        _ -> return $ foldl1 AppExpr atoms

atomExpr :: Parser Expr
atomExpr = try $ choice
    [ litExpr
    , signedlitExpr
    , boolExpr
    , letExpr
    , varExpr
    , listExpr
    , charExpr
    , strExpr
    , (parens $ expr)
    ]

charExpr :: Parser Expr
charExpr = do
    symbol "'"
    c <- anyChar
    symbol "'"
    return $ CharExpr c

letExpr :: Parser Expr
letExpr = do
    reserved "let"
    id <- identifier
    reservedOp ":"
    def <- expr
    reserved "in"
    e <- expr
    return $ LetExpr id def e

nilExpr :: Parser Expr
nilExpr = do
    symbol "["
    symbol "]"
    return $ ListExpr []
    
listExpr :: Parser Expr
listExpr = 
    (try nilExpr)
    <|> do
        elems <- squares (sepBy expr (reservedOp ","))
        return $ ListExpr elems

strExpr :: Parser Expr
strExpr = do
    str <- stringLiteral
    return $ StrExpr str

boolExpr :: Parser Expr
boolExpr = do{ reserved "true"; return $ BoolExpr True}
    <|> do { reserved "false"; return $ BoolExpr False}

litExpr :: Parser Expr
litExpr = do
    i <- natural
    return $ LitExpr (fromIntegral i)

signedlitExpr :: Parser Expr
signedlitExpr = do
    symbol "-"
    i <- natural
    return $ LitExpr (fromIntegral (-i))

varExpr :: Parser Expr
varExpr = do
    id <- identifier
    return $ VarExpr id

binding :: Parser Binding
binding = banonym <|> bnumber <|> bbool <|> bvar <|> bheadtail <|> bnil <|> bstring

bnil :: Parser Binding
bnil = do
    symbol "["
    symbol "]"
    return $ BNil

bstring :: Parser Binding
bstring = do
    str <- stringLiteral 
    return $ BString str
        
bheadtail :: Parser Binding
bheadtail = do
    parens $ do
        head <- binding
        reservedOp ":"
        tail <- binding
        return $ BList (head,tail)

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
