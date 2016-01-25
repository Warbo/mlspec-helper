{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, KindSignatures, ScopedTypeVariables, TemplateHaskell #-}
module MLSpec.Helper
       ( module MLSpec.Helper
       , module IfCxt
       ) where

import Control.Monad
import Data.Char
import Data.Generics.Aliases
import Data.Typeable
import Data.Word
import IfCxt
import Language.Haskell.TH.Syntax
import RuntimeArbitrary
import System.IO
import System.IO.Unsafe
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickSpec
import Test.QuickSpec.Signature
import Test.QuickSpec.Utils.Typed
import Data.Constraint
import Control.Exception

mono = fmap vToC . monomorphic

vToC (VarE n) = if isC n then ConE n else VarE n
vToC (ConE n) = if isC n then ConE n else VarE n
vToC x        = x

isC n = let (c:_) = nameBase n
         in isUpper c || c `elem` ":["

addVars :: Sig -> Sig
addVars sig = signature (sig : vs)
  where vs :: [Sig]
        vs = [gvars (names (Some w)) gen |
                  Some w <-        argumentTypes sig
                , Some w `notElem` variableTypes sig
                , gen    <-        getArbGen w
                ]
        names w = let n = show (witnessType w)
                   in ["var(" ++ n ++ ", 1)",
                       "var(" ++ n ++ ", 2)",
                       "var(" ++ n ++ ", 3)"]
