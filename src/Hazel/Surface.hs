module Hazel.Surface where

import Data.Text (Text)
import Data.Vector (Vector)

data Computation
  = Var Text
  | App Computation Value
  | Annot Value PreType
  | Case Computation     -- ^ expression
         PreType
         (Vector Value)  -- ^ expressions for each index

  | Choose
      Computation -- expression
      Int         -- index of field to access

  | Unpack (Vector Text) Computation (Value, PreType)

data PrimTy
  = StringTy
  | NatTy

data PreType
  = PrimTy PrimTy
  | IndexTy Int
  | LollyTy (PreType, UsageDeclaration) PreType
  | TupleTy (Vector PreType)
  | TimesTy (Vector PreType)
  | WithTy (Vector PreType)
  | PlusTy (Vector PreType)

-- checked terms / introductions / values
data Value
  = Lam Text Value
  | Primop Primop
  | Let UsageDeclaration Computation Value
  | Index Int
  | Primitive Primitive
  | Neu Computation
  | Tuple (Vector Value)
  | Plus Int Value

data Primitive
  = String String
  | Nat Int

-- TODO(joel) these are going away eventually
data UsageDeclaration
  = Irrelevant
  | Linear
  | Inexhaustible

data Primop
  = Add
  | PrintNat
  | ConcatString
  | ToUpper
  | ToLower
