{-# LANGUAGE OverloadedStrings #-}
module Spec.Parse where

import qualified Data.Vector as V
import Test.Hspec
import Text.Megaparsec (parseMaybe)

import Hazel.Parse
import Hazel.Surface

nat :: PreType
nat = PrimTy NatTy

test :: IO ()
test = hspec $ do
  describe "1 + 1" $ do
    let addTy = LollyTy (TimesTy (V.fromList [nat, nat]), Linear) nat
        one = Primitive (Nat 1)
        parsed = App (App (Annot (Primop Add) addTy) one) one
    it "parses" $ parseMaybe computation "1 + 1" `shouldBe` Just parsed
