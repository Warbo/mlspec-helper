{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, KindSignatures, ScopedTypeVariables, MultiParamTypeClasses #-}
module MLSpec.Helper where

import Data.Char
import Data.Typeable
import IfCxt
import Language.Haskell.TH.Syntax
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickSpec
import Test.QuickSpec.Signature
import Test.QuickSpec.Utils.Typed

mono = fmap vToC . monomorphic

vToC (VarE n) = if isC n then ConE n else VarE n
vToC (ConE n) = if isC n then ConE n else VarE n
vToC x        = x

isC n = let (c:_) = nameBase n
         in isUpper c || c `elem` ":["

getArb :: forall a. (Typeable a, IfCxt (Arbitrary a)) => a -> Gen a
getArb x = ifCxt (Proxy::Proxy (Arbitrary a))
  arbitrary
  (error "No arbitrary instance")

addVars :: Sig -> Sig
addVars sig = signature (sig : vs)
  where vs :: [Sig]
        vs = [gvars (names (witness w)) (getArb w) |
                Some w <-         argumentTypes sig,
                Some w `notElem`  variableTypes sig]
        names x = let n = show (typeRep [x])
                   in [n ++ "1", n ++ "2", n ++ "3"]

{-
isArbitrary :: Name -> Bool
isArbitrary n =

arbInstances ::

makeVars :: Name -> ExpQ
makeVars n = reify n
-}
