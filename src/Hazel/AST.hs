{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Hazel.AST where

import Hazel.Var

import Data.Vector (Vector, (!))
import Data.Text (Text)

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

----------------------

-- TODO: ViewPatterns or PatternSynonyms?
-- in 7.10 vs 8.0 which of  A => B => C  has A,B as given vs wanted / defult singleton
--- => case changes, see 8.0 migration notes etc for details

-- Might not be able to derive Foldable, etc. on GADTs?

-- Carter: might view patterns get you as far as indexing by direction?

data Direction = Infer | Check

type Computation = Term 'Infer
type Value       = Term 'Check

data Term (d :: Direction) where
  Var       :: Variable                            -> Computation
  App       :: Computation -> Value                -> Computation
  Annot     :: Value -> Type                       -> Computation
  Case      :: Computation -> Type -> Vector Value -> Computation -- TODO: remove type
  Choose    :: Computation -> Int                  -> Computation
  Unpack    :: Computation -> (Value, Type)        -> Computation -- TODO: what's really needed?
  Lam       :: Value                               -> Value
  Primop    :: Primop                              -> Value
  Let       :: Computation -> Type                 -> Value
  Index     :: Int                                 -> Value
  Primitive :: Primitive                           -> Value
  Neu       :: Computation                         -> Value
  Tuple     :: TupleModality -> Vector Value       -> Value       -- TODO: remove modality
  Plus      :: Int -> Value                        -> Value

-- data SomeTerm where
--   TermInfer :: Value -> SomeTerm
--   TermCheck :: Computation -> SomeTerm

--------------------- openC/openV unified via GADT:

open :: Vector Text -> Term d -> Term d
open names = go $ Depth 0
  where
    go :: Depth -> Term d -> Term d
    go k tm = case tm of
      Var (B i (Slot idx)) -> if i == k
                              then Var $ F $ names ! fromIntegral idx
                              else tm
      Var (F _) -> tm
      App fun arg -> App (go k fun) (go k arg)
      -- ...
      -- ...
      Lam bod -> Lam $ go (succ k) bod
      -- ...
      -- ...

----------------------- GADT with annotation baked into every case:

type Computation' = Term' 'Infer
type Value'       = Term' 'Check

data Term' (d :: Direction) a where
  Var'       :: a -> Variable                                    -> Computation' a
  App'       :: a -> Computation' a -> Value' a                  -> Computation' a
  Annot'     :: a -> Value -> Type                               -> Computation' a
  Case'      :: a -> Computation' a -> Type -> Vector (Value' a) -> Computation' a -- TODO: remove type
  Choose'    :: a -> Computation' a -> Int                       -> Computation' a
  Unpack'    :: a -> Computation' a -> (Value' a, Type)          -> Computation' a -- TODO: what's really needed?
  Lam'       :: a -> Value' a                                    -> Value' a
  Primop'    :: a -> Primop                                      -> Value' a
  Let'       :: a -> Computation' a                              -> Value' a
  Index'     :: a -> Int                                         -> Value' a
  Primitive' :: a -> Primitive                                   -> Value' a
  Neu'       :: a -> Computation' a                              -> Value' a
  Tuple'     :: a -> TupleModality -> Vector (Value' a)          -> Value' a       -- TODO: remove modality
  Plus'      :: a -> Int -> Value' a                             -> Value' a


-- TODO: try (what edward yang calls) "separate IR":
-- type TExp = (TExp', Type)
-- data TExp' = TNum Int
--            | TBool Bool
--            | TVar Var
--            | TIf TExp TExp TExp
--            | TLambda Var TExp
--            | TApp TExp TExp

-- TODO: look into ghc tying the knot

-- Carter: consider a non-indexed ADT with a sibling shadow GADT for indexing

data  TermModeEv :: Direction -> *  where
    Index'ValueEv :: TermModeEv Checked
    Unpack'Computation :: TermModeEv Infer
