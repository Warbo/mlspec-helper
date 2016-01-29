{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module MLSpec.Helper
       ( module MLSpec.Helper
       , module IfCxt
       ) where

import Control.Monad
import Data.Char
import Data.Generics.Aliases
import Data.List
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
import Test.QuickSpec.Term as T
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
        vs       = map (gvars names) gs
        names    = map mkName [1, 2, 3]
        mkName i = show n ++ show i

requiredVarTypes sig = [ someType (Some w)
                       | Some w <-        argumentTypes sig
                       , Some w `notElem` variableTypes sig]

showReqVarTypes = unlines . map show . requiredVarTypes

quickSpecRaw :: Sig -> IO [JSON]
quickSpecRaw sig = do
  r <- QS.Gen.generate False (const partialGen) sig
  let clss = concatMap (some2 (map (Some . O) . classes)) (TypeMap.toList r)
      univ = concatMap (some2 (map (tagged term))) clss
      reps = map (some2 (tagged term . head)) clss
      eqs  = equations clss
      allEqs = map (some (showEq sig)) eqs
  return allEqs

showEq :: Sig -> TypedEquation a -> JSON
showEq sig e = showEq' sig (eraseEquation e)

showEq' :: Sig -> Equation -> JSON
showEq' sig (t :=: u) = mkObj [
    (mkStr "relation", mkStr "~=")
  , (mkStr "lhs",      render t)
  , (mkStr "rhs",      render u)
  ]
  where render = showTerm . mapVars f
        f      = disambiguate sig (T.vars t ++ T.vars u)

showTerm :: Term -> JSON
showTerm t = case t of
  Var   s -> mkObj [
      (mkStr "role", mkStr "variable")
    , (mkStr "type", mkStr (show (symbolType s)))
    , (mkStr "id",   JSON (show (index s)))
    ]
  Const s -> mkObj [
      (mkStr "role", mkStr "constant")
    , (mkStr "symbol", mkStr (show s))
    ]
  App x y -> mkObj [
      (mkStr "role", mkStr "application")
    , (mkStr "lhs",  showTerm x)
    , (mkStr "rhs",  showTerm y)
    ]

-- Simple JSON emitters. Take care when quoting strings. Use aeson if you need
-- something more powerful.

newtype JSON = JSON { unJson :: String }

instance Show JSON where
  show (JSON s) = s

-- | Turns (k1,v1):(k2,v2):... into {k1:v1,k2:v2,...}
mkObj :: [(JSON, JSON)] -> JSON
mkObj xs = JSON $ "{" ++ intercalate "," (map attr xs) ++ "}"

mkStr :: String -> JSON
mkStr s = JSON (show s)

-- | Turns (k,v) into k:v
attr :: (JSON, JSON) -> String
attr (JSON k, JSON v) = k ++ ":" ++ v
