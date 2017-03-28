{-# LANGUAGE OverloadedStrings #-}
module Hazel.Tokenize where

import Control.Applicative ((<|>))
import qualified Data.HashSet as HashSet
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Parser.Token as P
import qualified Text.Parser.Token.Style as P
import Data.Attoparsec.Text

type TokenizeError = String

data Token
  = Quote Text
  | Operator Text
  | Identifier Text
  | Number Integer
  | Reserved Text
  deriving (Eq, Show)

instance IsString Token where

reservedNames :: [Text]
reservedNames =
  [ "case"
  , "of"
  , "choose"
  , "unpack"
  , "from"
  , "print"
  , "toUpper"
  , "toLower"
  , "string"
  , "nat"
  ]

idStyle = P.haskellIdents { P._styleReserved = HashSet.fromList (T.unpack <$> reservedNames) }
identifier  = P.ident idStyle
reserved  = P.reserve idStyle

mkReserved :: (Monad m, P.TokenParsing m) => Text -> m Token
mkReserved name = Reserved name <$ reserved (T.unpack name)

operator :: (Monad m, P.TokenParsing m) => m Text
operator = undefined

quoted :: (Monad m, P.TokenParsing m) => m Text
quoted = undefined

token :: (Monad m, P.TokenParsing m) => m Token
token = Operator <$> operator
    <|> Number <$> P.integer
    <|> Quote <$> quoted
    <|> choice (mkReserved <$> reservedNames)
    <|> Identifier <$> identifier

tokenize :: Text -> Result [Token]
tokenize = parse (many1 token)

-- lexer :: P.TokenParser st
-- lexer = P.makeTokenParser haskellDef

-- tokenizeGrammar :: E.Grammar r (E.Prod r TokenizeError Char Token)
-- tokenizeGrammar = mdo
--   let ident = Identifier . T.pack <$> (
--         (:) <$> E.satisfy Char.isAlpha
--             <*> many (E.satisfy Char.isAlphaNum)
--             <?> "identifier"
--         )

--       num = Number . read <$> some (E.satisfy Char.isDigit) <?> "number"

--       -- intentionally dumb -- doesn't yet handle escaped quotes
--       str = E.token '"' *> E.satisfy (/= '"') <* E.token '"'

--   whitespace <- E.rule $
--     many $ E.satisfy (T.all Char.isSpace)

--   E.rule $ ident <|> num

-- tokenizer :: E.Parser TokenizeError Text Token
-- tokenizer = E.parser tokenizeGrammar

-- tokenizes :: Text -> ([Token], E.Report TokenizeError Text)
-- tokenizes = E.fullParses tokenizer
