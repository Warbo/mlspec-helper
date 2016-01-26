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
import Test.QuickSpec.Equation
import qualified Test.QuickSpec.Generate as QS.Gen
import Test.QuickSpec.Main
import Test.QuickSpec.Signature
import Test.QuickSpec.Term
import Test.QuickSpec.TestTree
import Test.QuickSpec.Utils.Typed
import qualified Test.QuickSpec.Utils.TypeMap as TypeMap
import Data.Constraint
import Control.Exception

mono = fmap vToC . monomorphic

vToC (VarE n) = if isC n then ConE n else VarE n
vToC (ConE n) = if isC n then ConE n else VarE n
vToC x        = x

isC n = let (c:_) = nameBase n
         in isUpper c || c `elem` ":["

addVars :: (Typeable a) => String -> [Gen a] -> Sig -> Sig
addVars n gs sig = signature (sig : vs)
  where vs :: [Sig]
        vs = map (gvars names) gs
        names = ["var(" ++ n ++ ", 1)",
                 "var(" ++ n ++ ", 2)",
                 "var(" ++ n ++ ", 3)"]

requiredVarTypes sig = [ someType (Some w)
                       | Some w <-        argumentTypes sig
                       , Some w `notElem` variableTypes sig]

showReqVarTypes = unlines . map show . requiredVarTypes

quickSpecRaw :: Signature a => a -> IO ()
quickSpecRaw = runTool $ \sig -> do
  r <- QS.Gen.generate False (const partialGen) sig
  let clss = concatMap (some2 (map (Some . O) . classes)) (TypeMap.toList r)
      univ = concatMap (some2 (map (tagged term))) clss
      reps = map (some2 (tagged term . head)) clss
      eqs  = equations clss
      allEqs = map (some (showTypedEquation sig)) eqs
  putStrLn (unlines allEqs)
  return ()
