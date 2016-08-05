{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Hazel.CofreeAST where

import Hazel.Var

import Data.Vector (Vector)

data Usage
  = Inexhaustible
  | UseOnce
  | Exhausted
  deriving (Eq, Show)

data Primitive
  = String String
  | Nat Int
  deriving (Eq, Show)

pattern NatV i = Primitive (Nat i)
pattern StrV s = Primitive (String s)

data Primop
  = Add
  | PrintNat
  | ConcatString
  | ToUpper
  | ToLower
  deriving (Eq, Show)

data PrimTy
  = StringTy
  | NatTy
  deriving (Eq, Show)

data Type
  = PrimTy PrimTy
  -- | Type of a bounded index.
  --
  -- `IndexTy 2` is the type of `Index 0` and `Index 1`. This type is analogous
  -- to `Fin` in that it describes bounded nats.
  | IndexTy Int
  | LollyTy (Type, Usage) Type
  | TupleTy (Vector Type)
  | TimesTy (Vector Type)
  | WithTy (Vector Type)
  | PlusTy (Vector Type)
  deriving (Eq, Show)

-- | How a tuple can be used.
data TupleModality
  = Nonlinear     -- ^ Can be accessed by pattern matching and field access as
                  -- often as you like.
  | LinearUnpack  -- ^ Eliminated exactly once by pattern matching all fields.
  | LinearProject -- ^ Eliminated exactly once by field access on one field.
  deriving (Eq, Show)

---------------------- Direction-indexed initial algebra

data Direction = Infer | Check

type Computation = Term 'Infer
type Value       = Term 'Check

data Term (d :: Direction) (t :: Direction -> *) where
  Var       :: Variable                              -> Computation t
  App       :: t 'Infer -> t 'Check                  -> Computation t
  Annot     :: t 'Check -> Type                      -> Computation t
  Case      :: t 'Infer -> Type -> Vector (t 'Check) -> Computation t -- TODO: remove type
  Choose    :: t 'Infer -> Int                       -> Computation t
  Unpack    :: t 'Infer -> (t 'Check, Type)          -> Computation t -- TODO: what's really needed?
  Lam       :: t 'Check                              -> Value t
  Primop    :: Primop                                -> Value t
  Let       :: t 'Infer -> Type                      -> Value t
  Index     :: Int                                   -> Value t
  Primitive :: Primitive                             -> Value t
  Neu       :: t 'Infer                              -> Value t
  Tuple     :: TupleModality -> Vector (t 'Check)    -> Value t       -- TODO: remove modality
  Plus      :: Int -> t 'Check                       -> Value t

newtype Tied dir = Tie (Term dir Tied)    -- Adapted from Fix

data Ann a dir = Ann a (Term dir (Ann a)) -- Adapted from Cofree

-- Unannotated AST examples:
--
-- let f = Var $ B (Depth 0) (Slot 0) :: Computation t
-- let x = Var $ B (Depth 1) (Slot 0) :: Computation t
-- Tie f :: Tied Infer
-- Tie x :: Tied Infer
-- Neu $ Tie x :: Value Tied
-- Tie $ Neu $ Tie x :: Tied Check
-- App (Tie f) (Tie $ Neu $ Tie x) :: Computation Tied
-- Tie $ App (Tie f) (Tie $ Neu $ Tie x) :: Tied Infer
