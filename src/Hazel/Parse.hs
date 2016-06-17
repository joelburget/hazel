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

      vec p = V.fromList <$> many p

  computation <- E.rule $
        Var <$> ident
    <|> App <$> computation <*> value
    -- <|> Annot <$> value <* (":" :: E.Prod r ParseError Text Text) <*> preType
    <|> Annot <$> value <* E.token ':' <*> preType

    -- case x goes to ty of
    --   rhs0
    --   rhs1
    <|> (do
      E.list "case"
      c <- computation
      E.list "goes to"
      ty <- preType
      E.list "of"
      vs <- vec value
      return (Case c ty vs)
    )
    <|> Choose <$> computation <*> nat
    <|> Unpack <$> computation <*> ((,) <$> value <*> preType)
    <?> "computation"

  value <- E.rule $
        Lam <$> value
    <|> Index <$> nat
    <|> Neu <$> computation
    <?> "value"

  preType <- E.rule $
    undefined
    <?> "pre-type"

  return computation
