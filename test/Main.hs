{-# LANGUAGE TemplateHaskell, FlexibleInstances, OverloadedStrings, PartialTypeSignatures #-}

module Main where

import           Data.Aeson
import           Data.List
import           Data.Maybe
import qualified Data.Stringable as S
import           Data.Typeable
import           MLSpec.Helper
import           RuntimeArbitrary
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Property
import           Test.QuickSpec
import           Test.QuickSpec.Signature
import           Test.QuickSpec.Term
import           Test.QuickSpec.Utils.Typeable (unTypeRep)
import           Test.QuickSpec.Utils.Typed
import           Test.QuickSpec.Utils.TypeRel
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

mkIfCxtInstances ''Ord
mkIfCxtInstances ''Arbitrary

main = defaultMain $ testGroup "All tests" [
    testProperty "Have Bool generator"             haveBoolGen
  , testProperty "Have Integer generator"          haveIntGen
  , testProperty "Bool generator works"            haveArbitraryBools
  , testProperty "Integer generator works"         haveArbitraryInts
  , testProperty "Arbitrary Integers are distinct" intsAreDistinct
  , testProperty "Arbitrary Bools are distinct"    boolsAreDistinct
  , testProperty "Can generate nonzero Integers"   getNonZeroInts
  , testProperty "Can check for variable types"    varCheckingWorks
  , testProperty "Can get argument types out"      haveArgTypes
  , testProperty "Argument types are correct"      checkArgTypes
  , testProperty "Can get variable types out"      haveVariableTypes
  , testProperty "Bool variables added"            getBoolVars
  , testProperty "Integer variables added"         getIntVars
  , testProperty "Get equations from Bool"         getBoolEqs
  , testProperty "All equations are valid JSON"    allEqsAreJson
  , testProperty "Expected equations are found"    foundExpectedEquations
  ]

haveBoolGen = not (null boolGens)
haveIntGen  = not (null intGens)

-- `getArbGen foo !! bar` should behave the same as `arbitrary`. We check both,
-- to ensure the test is working as intended.
haveArbitraryBools = not (null boolGens) ==> distinctVals boolGen
boolsAreDistinct   =                         distinctVals (arbitrary :: Gen Bool)

haveArbitraryInts = not (null intGens) ==> distinctVals intGen
intsAreDistinct   =                        distinctVals (arbitrary :: Gen Integer)

getNonZeroInts :: Integer -> Bool
getNonZeroInts 0 = discard
getNonZeroInts n = True

varCheckingWorks :: Int -> [String] -> Bool
varCheckingWorks x names = if null names then not found else found
  where sig = signature [gvars names (return x)]
        found = hasVarType sig (0 :: Int)

haveArgTypes = 1 === length [() | Some w <- argumentTypes boolSig']

checkArgTypes = typeRep [True] `elem` typeReps
  where argTypes = argumentTypes boolSig'
        typeReps = map (unTypeRep . witnessType) argTypes

haveVariableTypes s = expected === length [() | Some w <- variableTypes sig]
  where expected = if null s then 0 else 1
        sig      = gvars s (return True)

getBoolVars = boolSig `hasVarType` (undefined :: Bool)
getIntVars  = intSig  `hasVarType` (undefined :: Integer)

getBoolEqs = monadicIO $ do
  eqs <- run $ quickSpecRaw boolSig
  assert (not (null eqs))

allEqsAreJson = monadicIO $ do
  eqs <- run $ quickSpecRaw boolSig
  monitor (counterexample (unlines . map show $ eqs))
  assert (all isJust (map dec eqs))
  where dec :: JSON -> Maybe Value
        dec (JSON s) = decode (S.fromString s)

foundExpectedEquations = monadicIO $ do
    eqs <- run $ quickSpecRaw boolSig
    mapM (map dec eqs `contains`) expectedEquations
  where

-- Helpers

dec :: JSON -> Value
dec = fromJust . decode . S.fromString . unJson

contains :: [Value] -> (Value, Value) -> _
contains xs (x, y) = do
  let r = any (equates x y) xs
  if r then return ()
       else debug $ concat [
         "(",  S.toString (encode x), ", ", S.toString (encode y), ") ",
         "not in\n", unlines (map (S.toString . encode) xs)]
  assert r

equates x y e = e `elem` [eq x y, eq y x]

debug = monitor . counterexample

eq x y = dec . JSON . concat $ [
    "{\"relation\":\"~=\",\"lhs\":"
  , S.toString (encode x)
  , ",\"rhs\":"
  , S.toString (encode y)
  , "}"
  ]

-- | A list of terms which we should find equations for
expectedEquations = [(and01, and10)]
  where and01 = dec (JSON (app (app and (var 0)) (var 1)))
        and10 = dec (JSON (app (app and (var 1)) (var 0)))
        app :: String -> String -> String
        app x y = "{\"role\": \"application\",\"lhs\":" ++ x ++ ", \"rhs\":" ++ y ++ "}"
        and     = "{\"role\": \"constant\",\"symbol\":\"(Data.Bool.&&)\"}"
        var n   = "{\"type\":\"Bool\",\"role\":\"variable\",\"id\":" ++ show n ++ "}"

hasVarType :: (Typeable a) => Sig -> a -> Bool
hasVarType sig x = let vars  = variables sig
                       found = Test.QuickSpec.Utils.TypeRel.lookup x vars
                    in not (null found)

-- Make sure a `Gen a` isn't just `return foo` (we need to resize it first)
distinctVals :: (Eq a, Show a) => Gen a -> Property
distinctVals gen = forAll genList distinct
  where genList     = resize 1000 $ vectorOf 100 gen
        distinct xs = length (nub xs) >= 2

boolSig' = signature ["not" `fun1` not, "Data.Bool.&&" `fun2` (&&)]
boolSig  = addVars "Bool" (getArbGen [undefined :: Bool]) boolSig'

intSig' = signature ["fromInteger" `fun1` (fromInteger :: Integer -> Int)]
intSig  = addVars "Integer" (getArbGen [undefined :: Integer]) intSig'

boolGens = getArbGen (Witness (undefined :: Bool))
intGens  = getArbGen (Witness (undefined :: Integer))

-- We should have (at least) one generator; might as well test all of them
boolGen = oneof boolGens
intGen  = oneof intGens

failure = MkProperty (return (MkProp (MkRose failed [])))
