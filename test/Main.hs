{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Main where

import           Data.List
import           Data.Typeable
import           MLSpec.Helper
import           RuntimeArbitrary
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Property
import           Test.QuickSpec
import           Test.QuickSpec.Signature
import           Test.QuickSpec.Term
import           Test.QuickSpec.Utils.Typeable (unTypeRep)
import           Test.QuickSpec.Utils.Typed
import           Test.QuickSpec.Utils.TypeRel
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

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

-- Helpers

hasVarType :: (Typeable a) => Sig -> a -> Bool
hasVarType sig x = let vars  = variables sig
                       found = Test.QuickSpec.Utils.TypeRel.lookup x vars
                    in not (null found)

-- Make sure a `Gen a` isn't just `return foo` (we need to resize it first)
distinctVals :: (Eq a, Show a) => Gen a -> Property
distinctVals gen = forAll genList distinct
  where genList     = resize 1000 $ vectorOf 100 gen
        distinct xs = length (nub xs) >= 2

boolSig' = signature ["not" `fun1` not]
boolSig  = addVars boolSig'

intSig' = signature ["fromInteger" `fun1` (fromInteger :: Integer -> Int)]
intSig  = addVars intSig'

boolGens = getArbGen (Witness (undefined :: Bool))
intGens  = getArbGen (Witness (undefined :: Integer))

-- We should have (at least) one generator; might as well test all of them
boolGen = oneof boolGens
intGen  = oneof intGens

failure = MkProperty (return (MkProp (MkRose failed [])))
