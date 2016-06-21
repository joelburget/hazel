{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Hazel.Lex where

import qualified Data.Char as Char
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L


lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment blockComment

-- space consumer
sc :: Parser ()
sc = L.space (void $ char ' ') lineComment blockComment

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.integer

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws :: [Text] -- list of reserved words
rws = [
  "case", "->", "of",
  "choose",
  "unpack", "from",
  -- TODO these aren't really reserved words
  "+", "print", "++", "toUpper", "toLower"
  ]

identifier :: Parser Text
identifier = lexeme (p >>= check)
  where
    p       = T.pack <$> ((:) <$> letterChar <*> many alphaNumChar)
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

index :: Parser Int
index = do
  _ <- char '.'
  fromIntegral <$> L.integer
