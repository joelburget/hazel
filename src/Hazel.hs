{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
module Hazel where

-- Significant features of this treatment include:
-- * n-tuples as a generalization of 2-tuples. This is an easy optimization
--   win for little extra work.
-- * bidirectional checking. we're following the treatment from Guillaume
--   Allais' "Typing with Leftovers" with modifications.
-- * linearity
--
-- * working towards:
--   - explicit usage annotations, so we can have variables that are used
--     multiple times (or not at all)
--   - dependency
--   - desugaring records, variants, etc
--
-- Idea for good error messages for linearity: instead of a boolean `Usage`,
-- keep track of the usage sites. Also, we don't need to track usage for
-- non-linear variables when they're introduced.

-- TODO things to check:
-- * arities line up in all places
-- * threading contexts specifies calling convention in a very real way
-- * what data structures are we using (list vs vector / ralist)

import Control.Lens hiding (Const)
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (toUpper, toLower)
import Data.Vector (Vector)
import qualified Data.Vector as V


-- inferred terms / eliminations / neutral terms
data Computation
  = BVar Int
  | FVar String
  | App Computation Value

  -- Type annotations mark the places where computation is still to be done, or
  -- the "cuts". - I Got Plenty o' Nuttin'
  | Annot Value Type

  -- see below for connective eliminators

  -- questions arise re the eliminator for tuples
  -- * is it case, or was case just the eliminator for sums?
  -- * is let a suitable eliminator? but let's a checked term, not inferred
  -- * in a way, eliminating tuples is not computation! whereas functions and
  --   cases branch (justifying no inferred term for eliminating tuples).
  --
  -- ... actually we need case or there is no branching!
  --
  -- Introduction form: `Tuple Nonlinear` or `Tuple LinearUnpack`.
  | Case Computation     -- expression
         Type            -- type of the case expr
         (Vector Value)  -- expressions for each index

  | Choose
      Computation -- expression
      Int         -- index of field to access

  -- XXX(joel) this shouldn't be an instance of Eq -- I just made it so we
  -- could use == in the test spec
  deriving (Eq, Show)


-- | How a tuple can be used.
--
-- * 'Nonlinear': Can be accessed by pattern matching and field access as often
--   as you like.
-- * 'LinearUnpack': Eliminated exactly once by pattern matching all fields.
-- * 'LinearProject': Eliminated exactly once by field access on one field.
data TupleModality
  = Nonlinear
  | LinearUnpack
  | LinearProject
  deriving (Eq, Show)


-- checked terms / introductions / values
data Value
  = Lam Value
  | Primop Primop
  | Let Pattern Computation Value
  | Index Int
  | Primitive Primitive
  | Neu Computation

  -- | Representation for all product types.
  --
  -- Nonlinear: The familiar tuple you know and love. Use with 'Case' or 'Choose'
  -- LinearUnpack: 'Times'. Use with 'Case'.
  -- LinearProject: 'With'. Use with 'Choose'.
  --
  -- Use with 'Case'.
  | Tuple TupleModality (Vector Value)

  -- | Sum with case analysis.
  --
  -- Use with 'Case'.
  | Plus Int Value
  deriving (Eq, Show)

data Deriv
  -- computation
  = BVar'
  | FVar'
  | App1
  | App2
  | CaseArg
  | CaseBranch Int
  | ChooseArg
  | Annot'

  -- value
  | Lam'
  | Primop'
  | Let1
  | Let2
  | Index'
  | Primitive'
  | Neu'

  | Tuple' Int
  | Plus'
  deriving Show

-- Match nested n-tuples.
--
-- Easy extension: `Underscore` doesn't bind any variables. Useful?
data Pattern
  = MatchTuple (Vector Pattern)
  | MatchVar Usage
  deriving (Eq, Show)

-- floating point numbers suck http://blog.plover.com/prog/#fp-sucks
-- (so do dates and times)
data Primitive
  = String String
  | Nat Int
  deriving (Eq, Show)

pattern NatV i = Primitive (Nat i)
pattern StrV s = Primitive (String s)

-- We should think about a more extensible way to add primops to the language
-- -- there will probably be a registry mapping from a name to its type and
-- evaluator at some point, but for now this will work.
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
  | TimesTy (Vector Type)
  | WithTy (Vector Type)
  | PlusTy (Vector Type)
  deriving (Eq, Show)

data Usage = Inexhaustible | UseOnce | Exhausted
  deriving (Eq, Show)

useVar :: MonadError String m => LocationDirections -> Usage -> m Usage
useVar _ Inexhaustible = pure Inexhaustible
useVar _ UseOnce = pure Exhausted
useVar dirs Exhausted = throwStackError dirs "[useVar] used exhausted variable"

-- | Directions pointing to a location in the syntax tree.
--
-- This is used primarily error reporting -- we want to be able to show exactly
-- where errors appear.
--
-- This shows up in both type and linearity- checking.
type LocationDirections = [Deriv]

-- | The type and usage of each currently bound variable.
--
-- Unlike in "Typing with Leftovers" we bundle typing and usage together in the
-- same data structure. This list is de-bruijn indexed, so to get the type and
-- usage of `BVar i` we access `ctx !! i`.
type Ctx = [(Type, Usage)]

-- I (Joel) made the explicit choice to not use the state monad to track
-- leftovers, since I want to take a little more care with tracking linearity.
-- We layer it on in some places where it's helpful.
--
-- Similarly, I made the choice to not use the reader monad to handle
-- LocationDirections. By handling the directions parameter manually, we need
-- to consciously think about that parameter in every recursive call.
--
-- TODO do we need to check all linear variables have been consumed here?
-- * we should just fix this so it passes in the empty context to the checker
runChecker :: Either String Ctx -> String
runChecker = either id (const "success!")

assert :: MonadError String m => Bool -> LocationDirections -> String -> m ()
assert True _ _ = return ()
assert False dirs str = throwStackError dirs str

inferVar :: LocationDirections -> Ctx -> Int -> Either String (Ctx, Type)
inferVar dirs ctx k = do
  -- find the type, count this as a usage
  let (ty, usage) = ctx !! k
  usage' <- useVar dirs usage
  -- TODO(joel) this is the only line that uses lens -- remove it?
  return (ctx & ix k . _2 .~ usage', ty)

-- Type inference for primops is entirely non-dependent on the environment.
inferPrimop :: Primop -> Type
inferPrimop p =
  let nat = PrimTy NatTy
      str = PrimTy StringTy
      tuple = TimesTy . V.fromList
  in case p of
       Add -> LollyTy (tuple [nat, nat], UseOnce) nat
       PrintNat -> LollyTy (nat, UseOnce) str
       ConcatString -> LollyTy (tuple [str, str], UseOnce) str
       ToUpper -> LollyTy (str, UseOnce) str
       ToLower -> LollyTy (str, UseOnce) str


allTheSame :: (Eq a) => [a] -> Bool
allTheSame [] = True
allTheSame (x:xs) = and $ map (== x) xs

throwStackError :: MonadError String m => LocationDirections -> String -> m a
throwStackError dirs str =
  -- Note: we reverse dirs since we're using the list as a stack and we want
  -- the outermost to appear at the top and innermost to appear at the bottom.
  let stackStr = unlines (map show (reverse dirs))
  in throwError $ stackStr ++ "\n" ++ str

infer :: LocationDirections -> Ctx -> Computation -> Either String (Ctx, Type)
infer dirs ctx t = case t of
  BVar i -> inferVar (BVar':dirs) ctx i

  FVar _name -> throwStackError (FVar':dirs)
    "[infer FVar] found unexpected free variable"

  App cTm vTm -> do
    (leftovers, cTmTy) <- infer (App1:dirs) ctx cTm
    case cTmTy of
      -- XXX(joel) figure out this usage
      LollyTy (inTy, inUsage) outTy -> do
        leftovers2 <- check (App2:dirs) leftovers inTy vTm
        return (leftovers2, outTy)
      _ -> throwStackError (App1:dirs)
        "[infer App] inferred non LollyTy in LHS of application"

  Case cTm ty vTms -> do
    (leftovers, cTmTy) <- infer (CaseArg:dirs) ctx cTm

    -- TEMP(joel): right now we only case on indices -- generalize so we can
    -- handle sums rather than just enums.
    case cTmTy of
      IndexTy tm1Size -> assert
        -- Check that we have at least as many handlers as the size of the
        -- inspected term
        (tm1Size <= V.length vTms)
        (CaseArg:dirs)
        "[infer Case] index mismatch"
      _ -> throwStackError (CaseArg:dirs)
        "[infer Case] can't case on non-indices"

    branchCtxs <- imapM (\i vTm -> check (CaseBranch i:dirs) ctx ty vTm)
                        vTms

    assert (allTheSame (V.toList branchCtxs)) dirs
      "[infer Case] all branches must consume the same linear variables"

    return (V.head branchCtxs, ty)

  Choose cTm idx -> do
    (leftovers, cTmTy) <- infer (ChooseArg:dirs) ctx cTm

    case cTmTy of
      WithTy tys -> do
        assert (idx < V.length tys) (ChooseArg:dirs)
          "[infer Choose] index mismatch"
        let usage = snd (head ctx)
        -- TODO(joel) better api for variable use: strawman: pass in the index
        -- of the var you want to access, get back the new context.
        usage' <- useVar (ChooseArg:dirs) usage
        return (leftovers & _head . _2 .~ usage', tys V.! idx)
      _ -> throwStackError (ChooseArg:dirs)
        "[infer Choose] can't access non-With"

  Annot vTm ty -> do
    leftovers <- check (Annot':dirs) ctx ty vTm
    return (leftovers, ty)

check :: LocationDirections -> Ctx -> Type -> Value -> Either String Ctx
check dirs ctx ty tm = case tm of
  Lam body -> case ty of
    LollyTy (argTy, usage) tau -> do
      let bodyCtx = (argTy, usage):ctx
      (_, usage'):leftovers <- check (Lam':dirs) bodyCtx tau body
      assert (usage' /= UseOnce) (Lam':dirs)
        "[check Lam] must consume linear bound variable"
      return leftovers
    _ -> throwStackError (Lam':dirs)
      "[check Lam] checking lambda against non-lolly type"

  Primop p -> do
    let expectedTy = inferPrimop p
    assert (ty == expectedTy) dirs $
      "[check Primop] primop (" ++ show p ++ ") type mismatch"
    return ctx

  Let pat letTm vTm -> do
    (leftovers, tmTy) <- infer (Let1:dirs) ctx letTm
    patternTy <- typePattern pat tmTy
    -- XXX do we need to reverse these?
    let bodyCtx = patternTy ++ leftovers
        arity = length patternTy
    newCtx <- check (Let2:dirs) bodyCtx ty vTm

    -- Check that the body consumed all the arguments
    let (bodyUsage, leftovers2) = splitAt arity newCtx
    forM_ bodyUsage $ \(_ty, usage) ->
      assert (usage /= UseOnce) dirs
        "[check Let] must consume linear bound variables"
    return leftovers2

  Tuple modality vTms -> case ty of
    -- Thread the leftover context through from left to right.
    TimesTy tys -> do
      assert (modality == LinearUnpack) dirs
        "[check Tuple] tuple modality must be linear unpack for Times"

      -- Layer on a state transformer for this bit, since we're passing
      -- leftovers from one term to the next
      let calc = V.imapM
            (\i (tm', ty') -> do
              let dirs' = (Tuple' i):dirs
              leftovers <- get
              newLeftovers <- lift $ check dirs' leftovers ty' tm'
              put newLeftovers
            )
            (V.zip vTms tys)

      -- execState gives back the final state
      execStateT calc ctx


    -- make sure each of the branches checks and consumes the same resources
    WithTy tys -> do
      assert (modality == LinearProject) dirs
        "[check Tuple] tuple modality must be linear projection for With"

      allLeftovers <- flip V.imapM vTms $ \i val -> do
        check (Tuple' i:dirs) ctx (tys V.! i) val
      assert (allTheSame (V.toList allLeftovers)) dirs
        "[check WithTy] mismatching leftovers"
      return (V.head allLeftovers)

    _ -> throwStackError dirs
           "[check Tuple] checking Tuple against non-(TimesTy/WithTy)"

  Plus idx val -> case ty of
    PlusTy tys ->
      let subTy = tys V.! idx
      in check (Plus':dirs) ctx subTy val

    _ -> throwStackError dirs
           "[check Plus] checking Plus agains non-PlusTy"

  Primitive prim -> do
    case prim of
      String _ -> assert (ty == PrimTy StringTy) (Primitive':dirs)
        "[check Primitive] trying to match string against non-string type"
      Nat _ -> assert (ty == PrimTy NatTy) (Primitive':dirs)
        "[check Primitive] trying to match nat against non-nat type"
    return ctx

  Index i -> case ty of
    IndexTy size -> do
      assert (i < size) (Index':dirs)
        "[check Index] didn't find index in index vec"
      return ctx
    _ -> throwStackError (Index':dirs)
           "[check Index] checking Index against non-IndexTy"

  Neu cTm -> do
    (leftovers, cTmTy) <- infer (Neu':dirs) ctx cTm
    assert (cTmTy == ty) (Neu':dirs)
      "[check Neu] checking inferred neutral type"
    return leftovers

checkToplevel :: Type -> Value -> Either String Ctx
checkToplevel = check [] []

evalC :: [Value] -> Computation -> Either String Value
evalC env tm = case tm of
  BVar i -> pure (env !! i)
  FVar name -> throwError ("unexpected free var in evaluation: " ++ name)
  App cTm vTm -> do
    cTm' <- evalC env cTm
    vTm' <- evalV env vTm
    case cTm' of
      Lam cBody -> evalV (vTm':env) cBody
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
          _ -> throwError "[evalC Case] couldn't find branch"
      Tuple _ tupleTms -> do
        -- TODO(joel) it feels like this duplicates tuple evaluation
        tupleTms' <- forM tupleTms (evalV env)

        -- We want the tuple terms to be bound from left to right, so we
        -- reverse the value list
        let newEnv = V.toList (V.reverse tupleTms') ++ env

        when (V.length vTms /= 1) (throwError
          "[evalC Case] case for Tuple must have exactly one branch"
          )
        evalV newEnv (V.head vTms)
      _ -> throwError "[evalC Case] unmatchable"
  Choose cTm idx -> do
    cTm' <- evalC env cTm
    case cTm' of
      Tuple _ valueVec -> do
        let vTm = valueVec V.! idx
        evalV env vTm
      _ -> throwError "[evalC Choose] field not found"
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
evalPrimop x y = throwError ("unexpected arguments to evalPrimop: " ++ show (x, y))

evalV :: [Value] -> Value -> Either String Value
evalV env tm = case tm of
  -- TODO(joel) only force / evaluate one field if modality is LinearProject
  Tuple modality vTms -> Tuple modality <$> mapM (evalV env) vTms
  Plus _ _ -> throwError "[evalV Plus] evaluating uneliminated Plus"
  Let _pat cTm vTm -> do
    cTm' <- evalC env cTm
    evalV (cTm':env) vTm
  Primop _ -> pure tm
  Lam _ -> pure tm
  Primitive _ -> pure tm
  Index _ -> pure tm
  Neu cTm -> evalC env cTm

-- TODO we don't actually use the implementation of opening -- I had just
-- pre-emptively defined it thinking it would be used.
openC :: Int -> String -> Computation -> Computation
openC k x tm = case tm of
  BVar i -> if i == k then FVar x else tm
  FVar _ -> tm
  App cTm vTm -> App (openC k x cTm) (openV k x vTm)
  Case cTm ty vTms ->
    Case (openC k x cTm) ty (V.map (openV (k + 1) x) vTms)
  Choose cTm i -> Choose (openC k x cTm) i
  Annot vTm ty -> Annot (openV k x vTm) ty

openV :: Int -> String -> Value -> Value
openV k x tm = case tm of
  Lam vTm -> Lam (openV (k + 1) x vTm)
  Tuple modality vTms -> Tuple modality (V.map (openV k x) vTms)
  Plus i vTm -> Plus i (openV k x vTm)
  Let pat cTm vTm ->
    let bindingSize = patternSize pat
    in Let pat (openC k x cTm) (openV (k + bindingSize) x vTm)
  Index _ -> tm
  Primitive _ -> tm
  Primop _ -> tm
  Neu cTm -> Neu (openC k x cTm)

typePattern :: MonadError String m => Pattern -> Type -> m [(Type, Usage)]
typePattern (MatchVar usage) ty = pure [(ty, usage)]
-- TODO check these line up (subPats / subTys)
--
-- example:
-- (MatchVar, MatchTuple (MatchVar, MatchVar))
--           v
-- [[ty0], [ty1, ty2]]
typePattern (MatchTuple subPats) (TimesTy subTys) = do
  let zipped = V.zip subPats subTys
  twoLevelTypes <- mapM (uncurry typePattern) zipped
  return (concat twoLevelTypes)
typePattern _ _ = throwError "[typePattern] misaligned pattern"

patternSize :: Pattern -> Int
patternSize (MatchVar _0) = 1
patternSize (MatchTuple subPats) = sum (V.map patternSize subPats)
