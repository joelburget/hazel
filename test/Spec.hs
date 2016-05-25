module Main where

import qualified Data.Vector as V
import Test.Hspec

import Hazel

bVar :: Int -> Value
bVar = Neu . BVar

natTy, strTy, boolTy :: Type
natTy = PrimTy NatTy
strTy = PrimTy StringTy
boolTy = IndexTy 2

str :: String -> Value
str = Primitive . String

nat :: Int -> Value
nat = Primitive . Nat

-- \(x, y) -> (y, x)
swap :: Value
swap = Lam (Let
  (MatchTuple (V.fromList [MatchVar UseOnce, MatchVar UseOnce]))
  (BVar 0)
  (Tuple LinearUnpack (V.fromList [bVar 1, bVar 0]))
  )

-- let (x, y) = \z -> z in (x, y)
illTyped :: Value
illTyped = Let
  (MatchTuple (V.fromList [MatchVar UseOnce, MatchVar UseOnce]))
  (Annot (Lam (bVar 0)) (LollyTy (natTy, UseOnce) natTy))
  (Tuple LinearUnpack (V.fromList [bVar 0, bVar 1]))

-- \x -> (x, x)
diagonal :: Value
diagonal = Lam (Tuple LinearUnpack (V.fromList [bVar 0, bVar 0]))

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

-- > case (0, "string") of
--     (x, y) -> (printNat x, toUpper y)
-- ("0", "STRING")
timesExample :: Computation
timesExample =
  let tupleTy = TimesTy (V.fromList [natTy, strTy])
      tuple = Tuple LinearUnpack (V.fromList [nat 0, str "string"])
      f = Tuple LinearUnpack (V.fromList
        [ Neu (App
            (Annot (Primop PrintNat) (inferPrimop PrintNat))
            (bVar 1)
          )
        , Neu (App
            (Annot (Primop ToUpper) (inferPrimop ToUpper))
            (bVar 0)
          )
        ])
      branches = V.singleton f
  in Case (Annot tuple tupleTy) natTy branches

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
            (Tuple LinearUnpack (V.fromList [bVar 0, str "bar"]))
          )
        , Neu (App
            (Annot (Primop ToUpper) (inferPrimop ToUpper))
            (bVar 0)
          )
        ]
  in Case (Annot (Index 0) boolTy) strTy branches

main :: IO ()
main = hspec $ do
  describe "swap" $ do
    let swapTy = LollyTy
          (TimesTy (V.fromList [strTy, natTy]), UseOnce)
          (TimesTy (V.fromList [natTy, strTy]))
    it "checks" $ runChecker (checkToplevel swapTy swap) `shouldBe` "success!"

  describe "diagonal" $ do
    let diagonalTy =
          let x = PrimTy StringTy
          in LollyTy (x, UseOnce) (TimesTy (V.fromList [x, x]))
        expected = "Lam'\nTuple' 1\nNeu'\nBVar'\n\n[useVar] used exhausted variable"
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
    let checker = check [] [(strTy, UseOnce)] strTy (Neu plusExample)
        expected = Right (str "foobar")
    it "checks" $ runChecker checker `shouldBe` "success!"
    it "evaluates" $ evalC [str "foo"] plusExample `shouldBe` expected
