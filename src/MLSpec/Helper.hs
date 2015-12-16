{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, KindSignatures, ScopedTypeVariables, MultiParamTypeClasses, TemplateHaskell, ImpredicativeTypes, InstanceSigs #-}
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

getArbGen :: forall a. (Typeable a, IfCxt (Arbitrary a)) => a -> [Gen a]
getArbGen x = ifCxt (Proxy::Proxy (Arbitrary a))
                    [arbitrary]
                    (listArb x)

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

newtype GenList a = GL [Gen a]

instance Functor GenList where
  fmap f (GL xs) = GL (fmap (fmap f) xs)

instance Applicative GenList where
  pure a = GL [return a]
  GL xs <*> GL ys = GL (map (<*>) xs <*> ys)

instance Monad GenList where
  return = pure
  (>>=) :: GenList a -> (a -> GenList b) -> GenList b
  GL xs >>= f = result
    where --result :: GenList b
          result = GL [a]
          --a :: Gen b
          a = join b
          --b :: Gen (Gen b)
          b = fmap (\(GL ys) -> oneof ys) c
          --c :: Gen (GenList b)
          c = fmap f d
          --d :: Gen a
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

arbList :: [Char] -> GenList [Char]
arbList _ = GL [arbitrary]

arbM :: Maybe Word8 -> GenList (Maybe Word8)
arbM _ = GL [arbitrary]

arbGeneric :: (Typeable a) => a -> GenList a
arbGeneric x = GL (getArbGen x)

listArb :: (Typeable a) => a -> [Gen a]
listArb x = case lstArb x of
  GL xs -> xs

addVars :: Sig -> Sig
addVars sig = signature (sig : vs)
  where vs :: [Sig]
        vs = [gvars (names (witness w)) gen |
                  Some w <-         argumentTypes sig
                , Some w `notElem`  variableTypes sig
                , gen    <-         getArbGen (witness w)
                --, haveArb (witness w)
                ]
        names x = let n = show (typeRep [x])
                   in [n ++ "1", n ++ "2", n ++ "3"]
