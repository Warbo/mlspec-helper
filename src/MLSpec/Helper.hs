{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, TemplateHaskell, CPP #-}
module MLSpec.Helper
       ( module MLSpec.Helper
       , module IfCxt
       ) where

import Control.Monad
import Data.Char
import Data.Generics.Aliases
import Data.List
import Data.Stringable as S
import Data.Typeable
import Data.Word
import IfCxt
import Language.Haskell.TH
--import Language.Haskell.TH.Syntax
--import Language.Haskell.TH.Lib
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
import qualified Test.QuickSpec.Utils.TypeMap  as TypeMap
import qualified Test.QuickSpec.Utils.Typeable as Typeable
import Data.Constraint
import Control.Exception

mono = fmap vToC . monomorphic'

vToC (VarE n) = if isC n then ConE n else VarE n
vToC (ConE n) = if isC n then ConE n else VarE n
vToC x        = x

isC n = let (c:_) = nameBase n
         in isUpper c || c `elem` ":["

-- Adapted from Test.QuickCheck.All
monomorphic' :: Name -> ExpQ
monomorphic' t = do
  ty0 <- fmap infoType (reify t)
  ty' <- monoGo (pprint ty0) [] ty0
  case ty' of
    Nothing -> return (expName t)
    Just m  -> return (SigE (expName t) m)

monoGo err polys t0 = do
  (polys', ctx, ty) <- deconstructType' err t0
  case polys' ++ polys of
    [] -> return Nothing
    _  -> do
      ty' <- monomorphiseType' err (polys' ++ polys) ty
      return (Just ty')

expName :: Name -> Exp
expName n = if isVar n then VarE n else ConE n

isVar :: Name -> Bool
isVar = let isVar' (c:_) = not (isUpper c || c `elem` ":[")
            isVar' _     = True
         in isVar' . nameBase

infoType :: Info -> Type
#if __GLASGOW_HASKELL__ >= 711
infoType (ClassOpI _ ty _) = ty
infoType (DataConI _ ty _) = ty
infoType (VarI _ ty _) = ty
#else
infoType (ClassOpI _ ty _ _) = ty
infoType (DataConI _ ty _ _) = ty
infoType (VarI _ ty _ _) = ty
#endif

deconstructType' :: String -> Type -> Q ([(Name,Type)], Cxt, Type)
deconstructType' err ty0@(ForallT xs ctx ty) = do
  integer <- [t| Integer |]
  list    <- [t| []      |]
  let subIn (PlainTV  x)   = (x, integer)
      subIn (KindedTV x k) = case k of
        StarT -> (x, integer)
        (AppT (AppT ArrowT StarT) StarT) -> (x, list)
        _     -> error (err ++ " Higher-kinded type variables in type '" ++ pprint k ++ "'")
      force (_,_) = True
      force _     = False
  unless (all (force . subIn) xs) $ error (err ++ " Higher-kinded type variables in type")
  return (map subIn xs, ctx, ty)
deconstructType' _ ty = return ([], [], ty)

monomorphiseType' :: String -> [(Name, Type)] -> Type -> TypeQ
monomorphiseType' err polys ty = case ty of
  (VarT n)             -> case lookup n polys of
                               Just mono -> return mono
                               Nothing   -> error (err ++ (" No sub found for " ++ pprint n))
  (AppT t1 t2)         -> liftM2 AppT (monomorphiseType' err polys t1) (monomorphiseType' err polys t2)
  (ForallT xs cxt ty0) -> do ty' <- monoGo err polys ty
                             case ty' of
                                  Nothing -> return ty0 -- $ "Higher-ranked type"
                                  Just m  -> return m
  _                    -> return ty

addVars :: (Typeable a) => String -> [Gen a] -> Sig -> Sig
addVars n gs sig = signature (sig : vs)
  where vs :: [Sig]
        vs       = map (gvars names) gs
        names    = map mkName [1, 2, 3]
        mkName :: Int -> String
        mkName i = show n ++ show i

requiredVarTypes sig = [ Typeable.unTypeRep (someType (Some w))
                       | Some w <-        argumentTypes sig
                       , Some w `notElem` variableTypes sig]

showReqVarTypes = unlines . map (show . tyRepToJson) . requiredVarTypes

tyRepToJson :: TypeRep -> JSON
tyRepToJson t = case splitTyConApp t of
    (tc, xs)   -> mkObj [(mkStr "tycon", tyConToJson tc),
                         (mkStr "args",  mkArr (map tyRepToJson xs))]

tyConToJson :: TyCon -> JSON
tyConToJson x = mkObj [(mkStr "name",    mkStr (tyConName    x)),
                       (mkStr "module",  mkStr (tyConModule  x)),
                       (mkStr "package", mkStr (tyConPackage x))]

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

mkArr :: [JSON] -> JSON
mkArr xs = JSON $ "[" ++ intercalate "," (map unJson xs) ++ "]"

-- | Turns (k,v) into k:v
attr :: (JSON, JSON) -> String
attr (JSON k, JSON v) = k ++ ":" ++ v
