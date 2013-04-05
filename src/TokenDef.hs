module TokenDef 
    ( tokenDef
    )
  where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

tokenDef = am

am :: LanguageDef st
am = emptyDef
      { commentStart = "{-"
      , commentEnd = "-}"
      , commentLine = "--"
      , nestedComments = True
      , identStart = letter
      , identLetter = alphaNum <|> oneOf "_'"
      , opStart = opLetter am
      , opLetter = oneOf ":=\\->/|~.*[]"
      , reservedOpNames = ["::","=","\\","->","=>","/\\","\\/","|~|",".",":","*","[]",","]  
      , reservedNames = [ "fn", "if", "then", "else", "true", "false" ]
      , caseSensitive  = True
      }


