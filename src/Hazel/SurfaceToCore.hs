module Hazel.SurfaceToCore where

import Control.Lens hiding (Const)
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (toUpper, toLower)
import Data.Vector (Vector)

import qualified Data.Vector as V
import qualified Data.Text as T

import qualified Hazel.Surface as S
-- import qualified Hazel.Core as C
import Hazel.Core
import Hazel.Var

-- XXX(joel): this doesn't actually implement the lowering yet

-- TODO we don't actually use the implementation of opening -- I had just
-- pre-emptively defined it thinking it would be used.
openC :: Depth -> V.Vector T.Text -> Computation -> Computation
openC k names tm = case tm of
  Var (B i (Slot idx)) -> if i == k
                          then Var (F $ names V.! fromIntegral idx)
                          else tm
  Var (F _) -> tm
  App cTm vTm -> App (openC k names cTm) (openV k names vTm)
  Case cTm ty vTms ->
    Case (openC k names cTm) ty (V.map (openV k names) vTms)
  Choose cTm i -> Choose (openC k names cTm) i
  Unpack cTm (vTm, ty) ->
    Unpack (openC k names cTm) (openV (succ k) names vTm, ty)
  Annot vTm ty -> Annot (openV k names vTm) ty

openV :: Depth -> V.Vector T.Text -> Value -> Value
openV k x tm = case tm of
  Lam vTm -> Lam (openV (succ k) x vTm)
  Tuple modality vTms -> Tuple modality (V.map (openV k x) vTms)
  Plus i vTm -> Plus i (openV k x vTm)
  Let usageAnnot cTm vTm -> Let usageAnnot (openC k x cTm) (openV (succ k) x vTm)
  Index _ -> tm
  Primitive _ -> tm
  Primop _ -> tm
  Neu cTm -> Neu (openC k x cTm)
