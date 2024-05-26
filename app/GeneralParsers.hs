module GeneralParsers where

import Text.ParserCombinators.Parsec hiding (hexDigit, spaces)

parseEscaped :: Parser Char
parseEscaped = do
  char '\\'
  c <- oneOf "ntr\\\""
  return
    $ case c of
        'n' -> '\n'
        't' -> '\t'
        'r' -> '\r'
        '\\' -> '\\'
        '"' -> '"'

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^__~"
