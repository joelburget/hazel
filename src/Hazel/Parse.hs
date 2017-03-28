{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecursiveDo #-}

module Hazel.Parse where

import Control.Applicative (many, (<|>))
-- import Data.ListLike (cons)
import qualified Data.Vector as V
import qualified Text.Earley as E
import Text.Earley ((<?>))

import Hazel.Surface
import Hazel.Tokenize

type Position = (Int, Int)

type ParseError = String

-- data ParseError = ParseError
--   { position :: Deriv
--   , message :: Text
--   }

-- | The top-level 'Computation' parser.
expParser :: E.Parser ParseError [Token] Computation
expParser = E.parser grammar

grammar :: E.Grammar r (E.Prod r ParseError Token Computation)
grammar = mdo
  let plus = E.namedToken "+"
      minus = E.namedToken "-"
      arr = E.namedToken "->"
      vec p = V.fromList <$> many p

      index = E.token "." *> nat

  ident <- undefined
  nat <- undefined
  str <- undefined

  computation <- E.rule $
        Var <$> ident
    <|> App <$> computation <*> value
    -- <|> Annot <$> value <* (":" :: E.Prod r ParseError Text Text) <*> preType
    <|> Annot <$> value <* E.token ":" <*> preType

    -- case x -> ty of
    --   rhs0
    --   rhs1
    <|> (do
      _ <- E.token "case"
      c <- computation
      _ <- E.token "->"
      ty <- preType
      _ <- E.token "of"
      vs <- vec value
      return (Case c ty vs)
    )

    -- choose c.i
    <|> (do
      _ <- E.token "choose"
      c <- computation
      i <- index
      return (Choose c i)
      -- Choose <$> computation <*> index
    )

    -- unpack x, y, z from c in v : ty
    <|> (do
      _ <- E.token "unpack"
      params <- ident `sepBy` E.token ","
      _ <- E.token "from"
      c <- computation
      v <- value
      t <- preType

      return (Unpack (V.fromList params) c (v, t))
    )
    <?> "computation"

  value <- E.rule $
    -- \x -> v
        (do
      _ <- E.token "\\"
      name <- ident
      _ <- E.token "->"
      v <- value

      return (Lam name v)
    )
    <|> Primop <$> (
          Add <$ E.token "+"
      <|> PrintNat <$ E.token "print"
      <|> ConcatString <$ E.token "++"
      <|> ToUpper <$ E.token "toUpper"
      <|> ToLower <$ E.token "toLower"
    )
    -- TODO Let
    <|> Index <$> nat
    <|> Primitive <$> (String <$> str <|> Nat <$> nat)
    <|> Neu <$> computation
    -- TODO Tuple
    <|> Plus <$> (index <* E.token ":") <*> value
    <?> "value"

  preType <- E.rule $
    PrimTy <$> (
          StringTy <$ E.token "string"
      <|> NatTy <$ E.token "nat"
    )
    <|> IndexTy <$> nat
    <|> LollyTy <$> ((,) <$> preType <*> usageDecl)
                <*> preType
    -- <|> TupleTy
    <?> "pre-type"

  usageDecl <- E.rule $
       Irrelevant <$ E.token "0"
    <|> Linear <$ E.token "1"
    <|> pure Inexhaustible

  return computation

sepBy1 :: E.Prod r e t a -> E.Prod r e t sep -> E.Prod r e t [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

sepBy :: E.Prod r e t a -> E.Prod r e t sep -> E.Prod r e t [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | Get all full successful parses and a status report.
parses :: [Token] -> ([Computation], E.Report ParseError [Token])
parses = E.fullParses expParser
