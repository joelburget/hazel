module Main where

import Test.Hspec

import qualified Spec.Eval
import qualified Spec.Parse

main = do
  Spec.Eval.test
  Spec.Parse.test
