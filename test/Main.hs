module Main where

import           Data.List
import           Data.Typeable
import           MLSpec.Helper
import           Test.QuickCheck
import           Test.QuickCheck.Property
import           Test.QuickSpec
import           Test.QuickSpec.Signature
import           Test.QuickSpec.Term
import           Test.QuickSpec.Utils.Typed
import           Test.QuickSpec.Utils.TypeRel
import           Test.Tasty             (defaultMain, testGroup, localOption)
import           Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "Have arbitrary Bools"            haveArbitraryBools
  , testProperty "Have arbitrary Integers"         haveArbitraryIntegers
  , testProperty "Arbitrary Integers are distinct" integersAreDistinct
  , testProperty "Can generate nonzero Integers"   getNonZeroIntegers
  , testProperty "Can check for variable types"    varCheckingWorks
  , testProperty "Bool variables added"            getBoolVars
  --, testProperty "Integer variables added"         getIntegerVars
  ]

-- Bools have a hard-coded special case generator
haveArbitraryBools = if null gens
                        then failure
                        else forAll (vectorOf 100 (oneof gens)) distinctBools
  where gens = getArbGen (Witness (undefined :: Bool))
        distinctBools xs = True `elem` xs && False `elem` xs

-- Integer isn't hard-coded, but it should be picked up by ifcxt
haveArbitraryIntegers = if null gens
                           then failure
                           else forAll (vectorOf 100 (oneof gens))
                                       distinctIntegers
  where gens = getArbGen (Witness (undefined :: Integer))

-- getArbGen should return arbitrary, so we should get the same behaviour for
-- each
integersAreDistinct = forAll (vectorOf 100 arbitrary)
                             distinctIntegers

getNonZeroIntegers :: Integer -> Bool
getNonZeroIntegers 0 = discard
getNonZeroIntegers n = True

getBoolVars = sig `hasVarType` (undefined :: Bool)
  where sig = addVars (signature ["not" `fun1` not])

getIntegerVars = sig `hasVarType` (undefined :: Integer)
  where sig = addVars (signature ["fromInteger" `fun1` (fromInteger :: Integer -> Int)])

varCheckingWorks :: Int -> [String] -> Bool
varCheckingWorks x names = if null names then not found else found
  where sig = signature [gvars names (return x)]
        found = hasVarType sig (0 :: Int)

hasVarType :: (Typeable a) => Sig -> a -> Bool
hasVarType sig x = let vars  = variables sig
                       found = Test.QuickSpec.Utils.TypeRel.lookup x vars
                    in not (null found)

distinctIntegers :: [Integer] -> Bool
distinctIntegers xs = (length (nub xs) >= 2) || discard

failure = MkProperty (return (MkProp (MkRose failed [])))
