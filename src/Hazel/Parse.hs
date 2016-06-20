{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecursiveDo #-}

module Hazel.Parse where

import Control.Applicative (many, some, (<|>))
import Control.Monad.ST
import qualified Data.Char as Char
-- import Data.ListLike (cons)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Text.Earley as E
import Text.Earley ((<?>))

import Hazel.Surface
import Hazel.Var

type Position = (Int, Int)

type ParseError = String

-- data ParseError = ParseError
--   { position :: Deriv
--   , message :: Text
--   }

-- | Get all full successful parses and a status report.
parses :: Text -> ([Computation], E.Report ParseError Text)
parses = E.fullParses expParser

-- | The top-level 'Computation' parser.
expParser :: ST state
  (Text -> ST state (E.Result state ParseError Text Computation))
expParser = E.parser grammar

sepBy1 :: E.Prod r e t a -> E.Prod r e t sep -> E.Prod r e t [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

sepBy :: E.Prod r e t a -> E.Prod r e t sep -> E.Prod r e t [a]
sepBy p sep = sepBy1 p sep <|> pure []

grammar :: E.Grammar r (E.Prod r ParseError Char Computation)
grammar = mdo
  whitespace <- E.rule $
    many $ E.satisfy Char.isSpace

  let ident = T.pack <$> (
        (:) <$> E.satisfy Char.isAlpha
            <*> many (E.satisfy Char.isAlphaNum)
            <?> "identifier"
        )
      -- TODO(joel) - we probably want a tokenizer with text tokens instead of
      -- going char-by-char
      nat = read <$> some (E.satisfy Char.isDigit) <?> "nat"

      -- intentionally dumb -- doesn't yet handle escaped quotes
      str = E.token '"' *> many (E.satisfy (/= '"')) <* E.token '"'

      vec p = V.fromList <$> many p

      index = E.token '.' *> nat

  computation <- E.rule $
        Var <$> ident
    <|> App <$> computation <*> value
    -- <|> Annot <$> value <* (":" :: E.Prod r ParseError Text Text) <*> preType
    <|> Annot <$> value <* E.token ':' <*> preType

    -- case x -> ty of
    --   rhs0
    --   rhs1
    <|> (do
      _ <- E.list "case"
      c <- computation
      _ <- E.list "->"
      ty <- preType
      _ <- E.list "of"
      vs <- vec value
      return (Case c ty vs)
    )

    -- choose c.i
    <|> (do
      _ <- E.list "choose"
      c <- computation
      i <- index
      return (Choose c i)
      -- Choose <$> computation <*> index
    )

    -- unpack x, y, z from c in v : ty
    <|> (do
      _ <- E.list "unpack"
      params <- ident `sepBy` E.token ','
      _ <- E.list "from"
      c <- computation
      v <- value
      t <- preType

      return (Unpack (V.fromList params) c (v, t))
    )
    <?> "computation"

  value <- E.rule $
    -- \x -> v
        (do
      _ <- E.token '\\'
      name <- ident
      _ <- E.list "->"
      v <- value

      return (Lam name v)
    )
    <|> Primop <$> (
          Add <$ E.list "+"
      <|> PrintNat <$ E.list "print"
      <|> ConcatString <$ E.list "++"
      <|> ToUpper <$ E.list "toUpper"
      <|> ToLower <$ E.list "toLower"
    )
    -- TODO Let
    <|> Index <$> nat
    <|> Primitive <$> (String <$> str <|> Nat <$> nat)
    <|> Neu <$> computation
    -- TODO Tuple
    <|> (do
      i <- index
      v <- value
      return (Plus i v)
    )
    <?> "value"

  preType <- E.rule $
    undefined
    <?> "pre-type"

  usageDecl <- E.rule $
        Irrelevant <$ "0"
    <|> Linear <$ "1"
    <|> pure Inexhaustible

  return computation
