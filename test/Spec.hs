module Main where

import qualified Data.Vector as V
import Test.Hspec

import Hazel
import Hazel.Var

bVar :: Int -> Int -> Value
bVar d s = Neu . Var $ B (Depth $ fromIntegral d) (Slot $ fromIntegral s)

natTy, strTy, boolTy :: PreType
natTy = PrimTy NatTy
strTy = PrimTy StringTy
boolTy = IndexTy 2

str :: String -> Value
str = Primitive . String

nat :: Int -> Value
nat = Primitive . Nat

-- \(x, y) -> (y, x)
swap :: Value
swap = Lam (Neu $ Unpack (Var $ B (Depth 0) (Slot 0))
                         ( Tuple LinearUnpack (V.fromList [bVar 0 1, bVar 0 0])
                         , (TimesTy (V.fromList [natTy, strTy]))))


-- unpack (x, y) = \z -> z in (x, y)
illTyped :: Value
illTyped = Neu $ Unpack
  (Annot (Lam (bVar 0 0))
           (LollyTy (natTy, Linear) natTy))
  ( Tuple LinearUnpack (V.fromList [bVar 0 0, bVar 0 1])
  , (TimesTy (V.fromList [natTy, natTy])))

-- \x -> (x, x)
diagonal :: Value
diagonal = Lam (Tuple LinearUnpack (V.fromList [bVar 0 0, bVar 0 0]))

caseExample :: Computation
caseExample = Case
  (Annot (Index 0) (IndexTy 2))
  natTy
  (V.fromList [nat 1 , nat 2])

caseExample' :: Value
caseExample' = Neu caseExample

primopExample :: Computation
primopExample =
  let pair :: Value -> Value -> Value
      pair x y = Tuple LinearUnpack (V.fromList [x, y])
  in App (Annot (Primop ConcatString) (inferPrimop ConcatString))
         (pair (StrV "abc") (StrV "xyz"))

-- > unpack (x, y) = (0, "string")
--   in (printNat x, toUpper y)
-- ("0", "STRING")
timesExample :: Computation
timesExample =
  let tupleTy = TimesTy (V.fromList [natTy, strTy])
      tuple = Tuple LinearUnpack (V.fromList [nat 0, str "string"])
      f = Tuple LinearUnpack (V.fromList
        [ Neu (App
            (Annot (Primop PrintNat) (inferPrimop PrintNat))
            (bVar 0 0)
          )
        , Neu (App
            (Annot (Primop ToUpper) (inferPrimop ToUpper))
            (bVar 0 1)
          )
        ])
  in Unpack (Annot tuple tupleTy) (f, natTy)

-- > let x = "foo" in case False of
--     False -> concatString x "bar"
--     True -> toUpper x
-- "foobar"
--
-- (binding x in checking / evaluation context)
plusExample :: Computation
plusExample =
  let branches = V.fromList
        [ Neu (App
            (Annot (Primop ConcatString) (inferPrimop ConcatString))
            (Tuple LinearUnpack (V.fromList [bVar 0 0, str "bar"]))
          )
        , Neu (App
            (Annot (Primop ToUpper) (inferPrimop ToUpper))
            (bVar 0 0)
          )
        ]
  in Case (Annot (Index 0) boolTy) strTy branches

main :: IO ()
main = hspec $ do
  describe "swap" $ do
    let swapTy = LollyTy
          (TimesTy (V.fromList [strTy, natTy]), Linear)
          (TimesTy (V.fromList [natTy, strTy]))
    it "checks" $ runChecker (checkToplevel swapTy swap) `shouldBe` "success!"

  describe "diagonal" $ do
    let diagonalTy =
          let x = PrimTy StringTy
          in LollyTy (x, Linear) (TimesTy (V.fromList [x, x]))
        expected = "Lam'\nTuple' 1\nNeu'\nVar'\n\n[useVar] used exhausted variable"
        checker = checkToplevel diagonalTy diagonal
    it "doesn't Check" $ runChecker checker `shouldBe` expected

  describe "case" $ do
    let checker = checkToplevel natTy caseExample'
    it "checks" $ runChecker checker `shouldBe` "success!"
    it "evaluates" $ evalC [] caseExample `shouldBe` Right (nat 1)

  describe "primop" $ do
    let checker = checkToplevel strTy (Neu primopExample)
    it "checks" $ runChecker checker `shouldBe` "success!"
    it "evaluates" $ evalC [] primopExample `shouldBe` Right (str "abcxyz")

  describe "times" $ do
    let checker = checkToplevel strTy (Neu timesExample)
        expected = Right (Tuple
            LinearUnpack
            (V.fromList [Primitive (String "0"), Primitive (String "STRING")])
          )
    -- Currently disabled -- re-enable when merged with Brian's sum type
    -- changes.
    -- it "checks" $ runChecker checker `shouldBe` "success!"
    it "evaluates" $ evalC [] timesExample `shouldBe` expected

  describe "plus" $ do
    let checker = check [] [V.singleton (Type strTy One)] strTy (Neu plusExample)
        expected = Right (str "foobar")
    it "checks" $ runChecker checker `shouldBe` "success!"
    it "evaluates" $ evalC [V.singleton $ str "foo"] plusExample `shouldBe` expected
