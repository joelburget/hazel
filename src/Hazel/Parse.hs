{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecursiveDo #-}

module Hazel.Parse where

import Control.Applicative (many, some, (<|>))
import Control.Monad.ST
-- import Data.ListLike (cons)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Text

import Hazel.Surface
import Hazel.Lex
import Hazel.Var

type Position = (Int, Int)

type ParseError = String

-- data ParseError = ParseError
--   { position :: Deriv
--   , message :: Text
--   }

vec :: Parser a -> Parser (V.Vector a)
vec p = V.fromList <$> many p

computation :: Parser Computation
computation =
      Var <$> identifier
  <|> App <$> computation <*> value
  <|> Annot <$> value <* char ':' <*> preType

  -- case x -> ty of
  --   rhs0
  --   rhs1
  <|> (do
    _ <- rword "case"
    c <- computation
    _ <- rword "->"
    ty <- preType
    _ <- rword "of"
    vs <- vec value
    return (Case c ty vs)
  )

  -- choose c.i
  <|> (do
    _ <- rword "choose"
    c <- computation
    i <- index
    return (Choose c i)
    -- Choose <$> computation <*> index
  )

  -- unpack (x, y, z) from c in v : ty
  <|> (do
    _ <- rword "unpack"
    params <- parens $ identifier `sepBy` char ','
    _ <- rword "from"
    c <- computation
    v <- value
    t <- preType

    return (Unpack (V.fromList params) c (v, t))
  )
  <?> "computation"

value :: Parser Value
value = -- pLambda <|> pPrimop <|> pLet <|> pIndex <|> pPrimitive
  -- \x -> v
      (do
    _ <- char '\\'
    name <- identifier
    _ <- rword "->"
    v <- value

    return (Lam name v)
  )
  <|> Primop <$> (
        Add <$ rword "+"
    <|> PrintNat <$ rword "print"
    <|> ConcatString <$ rword "++"
    <|> ToUpper <$ rword "toUpper"
    <|> ToLower <$ rword "toLower"
  )
  -- TODO Let
  <|> Index <$> index
  <|> Primitive <$> (
        String <$> stringLiteral
    <|> Nat . fromIntegral <$> integer
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
