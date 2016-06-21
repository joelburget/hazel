{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecursiveDo #-}

module Hazel.Parse where

import Control.Applicative (many, some, (<|>))
import Control.Monad.ST
import qualified Data.Char as Char
import Data.Functor (void)
-- import Data.ListLike (cons)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L

import Hazel.Surface
import Hazel.Var

type Position = (Int, Int)

type ParseError = String

-- data ParseError = ParseError
--   { position :: Deriv
--   , message :: Text
--   }

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

ident :: Parser Text
ident = T.pack <$> (
      (:) <$> satisfy Char.isAlpha
          <*> many (satisfy Char.isAlphaNum)
          <?> "identifier"
      )

vec :: Parser a -> Parser (V.Vector a)
vec p = V.fromList <$> many p

index :: Parser Int
index = do
  _ <- char '.'
  fromIntegral <$> L.integer

computation :: Parser Computation
computation =
      Var <$> ident
  <|> App <$> computation <*> value
  <|> Annot <$> value <* char ':' <*> preType

  -- case x -> ty of
  --   rhs0
  --   rhs1
  <|> (do
    _ <- string "case"
    c <- computation
    _ <- string "->"
    ty <- preType
    _ <- string "of"
    vs <- vec value
    return (Case c ty vs)
  )

  -- choose c.i
  <|> (do
    _ <- string "choose"
    c <- computation
    i <- index
    return (Choose c i)
    -- Choose <$> computation <*> index
  )

  -- unpack x, y, z from c in v : ty
  <|> (do
    _ <- string "unpack"
    params <- ident `sepBy` char ','
    _ <- string "from"
    c <- computation
    v <- value
    t <- preType

    return (Unpack (V.fromList params) c (v, t))
  )
  <?> "computation"

value :: Parser Value
value =
  -- \x -> v
      (do
    _ <- char '\\'
    name <- ident
    _ <- string "->"
    v <- value

    return (Lam name v)
  )
  <|> Primop <$> (
        Add <$ string "+"
    <|> PrintNat <$ string "print"
    <|> ConcatString <$ string "++"
    <|> ToUpper <$ string "toUpper"
    <|> ToLower <$ string "toLower"
  )
  -- TODO Let
  <|> Index <$> index
  <|> Primitive <$> (
        String <$> stringLiteral
    <|> Nat . fromIntegral <$> L.integer
  )
  <|> Neu <$> computation
  -- TODO Tuple
  <|> (do
    i <- index
    v <- value
    return (Plus i v)
  )
  <?> "value"

preType :: Parser PreType
preType =
  undefined
  <?> "pre-type"

usageDecl :: Parser UsageDeclaration
usageDecl =
      Irrelevant <$ char '0'
  <|> Linear <$ char '1'
  <|> pure Inexhaustible
