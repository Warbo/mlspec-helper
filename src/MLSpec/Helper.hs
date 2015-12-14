{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, KindSignatures, ScopedTypeVariables, MultiParamTypeClasses, TemplateHaskell #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module MLSpec.Helper where

import Data.Char
import Data.Generics.Aliases
import Data.Typeable
import IfCxt
import Language.Haskell.TH.Syntax
import System.IO
import System.IO.Unsafe
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickSpec
import Test.QuickSpec.Signature
import Test.QuickSpec.Utils.Typed
import Data.Constraint
import Control.Exception
import System.IO.Unsafe

{-
getArb :: (Typeable a) => a -> Gen a
getArb x = unsafePerformIO $ do
    res <- try $ evaluate $ arb' Dict x
    case res of
        Right y             -> return y
        Left  (ErrorCall _) -> return (error ("No arbitrary for " ++ (show (typeRep [x]))))

arb' :: (Dict (Arbitrary a)) -> a -> Gen a
arb' Dict _ = arbitrary

haveArb :: (Typeable a) => a -> Bool
haveArb x = unsafePerformIO $ do
    res <- try $ evaluate $ arb' Dict x
    case res of
        Right y             -> return True
        Left  (ErrorCall _) -> return False
-}

--mkIfCxtInstances ''Arbitrary

mono = fmap vToC . monomorphic

vToC (VarE n) = if isC n then ConE n else VarE n
vToC (ConE n) = if isC n then ConE n else VarE n
vToC x        = x

isC n = let (c:_) = nameBase n
         in isUpper c || c `elem` ":["

{-
{-# NOINLINE getArb #-}
getArb :: forall a. (Typeable a, IfCxt (Arbitrary a)) => a -> Gen a
getArb x = ifCxt (Proxy::Proxy (Arbitrary a))
  arbitrary
  (unsafePerformIO (do hPutStrLn stderr ("No arbitrary for " ++ show (typeRep [x]))
                       return (error "No arbitrary instance")))
-}

getArb :: (Typeable a) => a -> Gen a
getArb = extM (\x -> error ("No generator for " ++ show (typeRep [x]))) $
           extM arbBool arbMaybe
  where arbBool :: Bool -> Gen Bool
        arbBool _ = arbitrary
        arbMaybe :: Maybe Char -> Gen (Maybe Char)
        arbMaybe _ = arbitrary

addVars :: Sig -> Sig
addVars sig = signature (sig : vs)
  where vs :: [Sig]
        vs = [gvars (names (witness w)) (getArb (witness w)) |
                  Some w <-         argumentTypes sig
                , Some w `notElem`  variableTypes sig
                --, haveArb (witness w)
                ]
        names x = let n = show (typeRep [x])
                   in [n ++ "1", n ++ "2", n ++ "3"]
