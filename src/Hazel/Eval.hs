module Hazel.Eval where

import Control.Lens hiding (Const)
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (toUpper, toLower)
import Data.Vector (Vector)

import qualified Data.Vector as V
import qualified Data.Text as T

import Hazel.Core
import Hazel.Var

evalC :: [V.Vector Value] -> Computation -> Either String Value
evalC env tm = case tm of
  Var var@(B _ _) -> maybe (throwError "var references non-existent slot")
                           return
                           (env ^? atVar var)
  Var (F name) -> throwError $
    "unexpected free var in evaluation: " ++ T.unpack name
  App cTm vTm -> do
    cTm' <- evalC env cTm
    vTm' <- evalV env vTm
    case cTm' of
      Lam cBody -> evalV (V.singleton vTm':env) cBody
      -- Note that we're passing in only the current heap value, not the
      -- context, since a primop must be atomic -- it can't capture
      Primop p -> evalPrimop p vTm'
      _ -> throwError "unexpected non lambda in lhs of function application"
  Case cTm _ty vTms -> do
    cTm' <- evalC env cTm
    case cTm' of
      Index branchIx ->
        case vTms V.!? branchIx of
          Just branch -> evalV env branch
          _ -> throwError
            ("[evalC Case] couldn't find branch for index " ++ show branchIx)
      _ -> throwError $ "[evalC Case] can't case on non-index " ++ show cTm'
  Choose cTm idx -> do
    cTm' <- evalC env cTm
    case cTm' of
      Tuple _ valueVec -> do
        let vTm = valueVec V.! idx
        evalV env vTm
      _ -> throwError "[evalC Choose] field not found"
  Unpack cTm (vTm, _ty) -> do
    cTm' <- evalC env cTm
    case cTm' of
      Tuple _ tupTms -> evalV (tupTms : env) vTm
      _ -> throwError $ "[evalC Unpack] can't unpack non-tuple: " ++ show cTm'
  Annot vTm _ty -> evalV env vTm


evalPrimop :: Primop -> Value -> Either String Value
evalPrimop Add (Tuple _modality args)
  | [NatV x, NatV y] <- V.toList args
  = pure (NatV (x + y))
evalPrimop PrintNat (NatV i) = pure (StrV (show i))
evalPrimop ConcatString (Tuple _modality args)
  | [StrV l, StrV r] <- V.toList args
  = pure (StrV (l ++ r))
evalPrimop ToUpper (StrV s) = pure (StrV (map toUpper s))
evalPrimop ToLower (StrV s) = pure (StrV (map toLower s))
evalPrimop x y = throwError
  ("unexpected arguments to evalPrimop: " ++ show (x, y))

evalV :: [V.Vector Value] -> Value -> Either String Value
evalV env tm = case tm of
  -- TODO(joel) only force / evaluate one field if modality is LinearProject
  Tuple modality vTms -> Tuple modality <$> mapM (evalV env) vTms
  Plus _ _ -> throwError "[evalV Plus] evaluating uneliminated Plus"
  Let _usage cTm vTm -> do
    cTm' <- evalC env cTm
    evalV (V.singleton cTm':env) vTm
  Primop _ -> pure tm
  Lam _ -> pure tm
  Primitive _ -> pure tm
  Index _ -> pure tm
  Neu cTm -> evalC env cTm
