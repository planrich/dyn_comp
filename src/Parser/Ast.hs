module Parser.Ast
    ( file
    )
  where

import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Pos

import Control.Monad

data Ast = Module [Ast]
         | Func Name Signature [Ast]
         | Expr Op Ast Ast
         | Pattern [Ast] [[Ast]]
         | Atom String
         | Number Integer
         | String String
         deriving (Show)

type Name = String
type Signature = [Type]

data Type = TInt
          | TString
          | TFunc Signature
          | TList Type
          deriving Show

data Op = Plus | Minus | Mul | Div deriving Show

file :: Parser [Ast]
file = many function >>= (\fs -> eof >> return fs)

function :: Parser Ast
function = do
    string "fn" >> ws1
    fname <- ident
    ws >> char ':' >> ws
    fnSig <- fnSignature
    nl
    patterns <- many1 pattern
    many (ws >> nl)
    return $ Func fname fnSig patterns

pattern :: Parser Ast
pattern = do
    ws
    char '='
    ws
    subpatterns <- many (patternArgument >>= (\x -> ws >> return x))
    ws >> char ';' >> ws
    pos <- getPosition
    expr' <- expression $ (sourceColumn pos) - 1
    return $ Pattern subpatterns expr'

expression :: Int -> Parser [[Ast]]
expression start = do
    formula <- sepBy1 (atom <|> str <|> number <|> op) ws1
    nl
    wsc <- ws
    if wsc == start
      then do e <- expression start
              return $ (formula:e)
      else return [formula]

patternArgument :: Parser Ast
patternArgument = atom <|> number <|> str

fnSignature :: Parser Signature
fnSignature = sepBy1 typ typSep

typ :: Parser Type
typ = do
          string "int"
          return TInt
      <|> do
          string "string"
          return TString
      <|> do
          char '['
          typ' <- typ
          char ']'
          return $ TList typ'

typSep :: Parser ()
typSep = ws >> string "->" >> ws >> return ()

number :: Parser Ast
number = liftM (Number . read) $ many1 digit

ident :: Parser String
ident = do
    head' <- underscore <|> letter
    tail' <- many (underscore <|> letter <|> digit)
    return (head':tail')

atom :: Parser Ast
atom = do
    head' <- underscore <|> letter
    tail' <- many (underscore <|> letter <|> digit)
    return $ Atom (head':tail')

str :: Parser Ast
str = do
    char '"'
    content <- many (noneOf "\"")
    char '"'
    return $ String content

op :: Parser Ast
op = do 
    o <- oneOf "+-*%"
    return $ Atom [o]

ws1 :: Parser Int
ws1 = do l <- many1 $ char ' '; return $ length l

ws :: Parser Int
ws = do l <- many $ char ' '; return $ length l

nl :: Parser ()
nl = (char '\n' <?> "newline") >> return ()

underscore :: Parser Char
underscore = char '_'


