{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, KindSignatures, ScopedTypeVariables, TemplateHaskell #-}
module MLSpec.Helper where

import Control.Monad
import Data.Char
import Data.Generics.Aliases
import Data.Typeable
import Data.Word
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

mkIfCxtInstances ''Arbitrary

mono = fmap vToC . monomorphic

vToC (VarE n) = if isC n then ConE n else VarE n
vToC (ConE n) = if isC n then ConE n else VarE n
vToC x        = x

isC n = let (c:_) = nameBase n
         in isUpper c || c `elem` ":["

getArbGen :: forall a. (Typeable a, IfCxt (Arbitrary a)) => Witnessed a -> [Gen a]
getArbGen x = ifCxt (Proxy::Proxy (Arbitrary a))
                    [arbitrary]
                    (listArb x)

{-
getArb :: (Typeable a) => a -> Gen a
getArb = extM (extM (extM (extM arbNope arbBool) arbMaybe) arbList) arbM
  where arbNope :: Typeable a => a -> Gen a
        arbNope x = error ("No generator for " ++ show (typeRep [x]))
        arbBool :: Bool -> Gen Bool
        arbBool _ = arbitrary
        arbMaybe :: Maybe Char -> Gen (Maybe Char)
        arbMaybe _ = arbitrary
        arbList :: [Char] -> Gen [Char]
        arbList _ = arbitrary
        arbM :: Maybe Word8 -> Gen (Maybe Word8)
        arbM _ = arbitrary
-}

newtype GenList a = GL [Gen a]

instance Functor GenList where
  fmap f (GL xs) = GL (fmap (fmap f) xs)

instance Applicative GenList where
  pure a = GL [return a]
  GL xs <*> GL ys = GL (map (<*>) xs <*> ys)

instance Monad GenList where
  return = pure
  GL xs >>= f = GL [a]
    where a = join b
          b = fmap (\(GL ys) -> oneof ys) c
          c = fmap f d
          d = oneof xs

gConcat [] = []
gConcat (GL xs:ys) = xs ++ gConcat ys

lstArb :: (Typeable a) => a -> GenList a
lstArb = extM (extM (extM (extM arbNope arbBool) arbMaybe) arbList) arbM

arbNope :: Typeable a => a -> GenList a
arbNope x = GL []

arbBool :: Bool -> GenList Bool
arbBool _ = GL [arbitrary]

arbMaybe :: Maybe Char -> GenList (Maybe Char)
arbMaybe _ = GL [arbitrary]

arbList :: String -> GenList String
arbList _ = GL [arbitrary]

arbM :: Maybe Word8 -> GenList (Maybe Word8)
arbM _ = GL [arbitrary]

listArb :: (Typeable a) => Witnessed a -> [Gen a]
listArb x = case lstArb (witness x) of
  GL xs -> xs

addVars :: Sig -> Sig
addVars sig = signature (sig : vs)
  where vs :: [Sig]
        vs = [gvars (names (Some w)) gen |
                  Some w <-        argumentTypes sig
                , Some w `notElem` variableTypes sig
                , gen    <-        getArbGen w
                --, haveArb (witness w)
                ]
        names w = let n = show (witnessType w)
                   in ["var(" ++ n ++ ", 1)",
                       "var(" ++ n ++ ", 2)",
                       "var(" ++ n ++ ", 3)"]
