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
  let subIn (PlainTV  x)   = (x, instantiateConstraintsDefault ctx x integer)
      subIn (KindedTV x k) = case k of
        -- a :: * defaults to Integer, unless we can guess from constraints
        StarT -> (x, instantiateConstraintsDefault ctx x integer)
        -- a :: * -> * defaults to [], unless we can guess from constraints
        (AppT (AppT ArrowT StarT) StarT) -> (x, instantiateConstraintsDefault ctx x list)
        -- No defaults for anything else, but we might be able to tell from constraints
        _     -> case snd (head (withConstraints ctx [x])) of
                      [] -> error (concat [
                              err,
                              " Couldn't guess instance for '",
                              pprint x,
                              "' of higher-kind '",
                              pprint k,
                              "'"])
                      _  -> (x, instantiateConstraintsDefault ctx x
                                  (error $ "No default for higher kind " ++ pprint k))
      force (_,_) = True
      force _     = False
  unless (all (force . subIn) xs) $ error (err ++ " Higher-kinded type variables in type")
  let xs'  = map subIn xs
      xs'' = mapM (\(x, y) -> y >>= (\y' -> return (x, y')))  xs'
  xs''' <- xs''
  return (xs''', ctx, ty)
deconstructType' _ ty = return ([], [], ty)

-- Switch this to get help hacking
templateDebug msg = unless True (reportWarning msg)

instantiateConstraintsDefault :: Cxt -> Name -> Type -> Q Type
instantiateConstraintsDefault cxt n def = do
    guesses <- satisfyAll cs
    templateDebug $ show ("cxt", cxt, "guesses", guesses, "n", n, "def", def)
    case guesses of
         (g:_) -> do templateDebug ("Choosing " ++ show (ConT g))
                     return (ConT g)
         []    -> do templateDebug ("Defaulting to " ++ show def)
                     return def
  where cs = snd (head (withConstraints cxt [n]))

monomorphiseType' :: String -> [(Name, Type)] -> Type -> TypeQ
monomorphiseType' err polys ty = case ty of
  (VarT n)             -> case lookup n polys of
                               Just mono -> return mono
                               Nothing   -> error (err ++ (" No sub found for " ++ pprint n))
  (AppT t1 t2)         -> liftM2 AppT (monomorphiseType' err polys t1) (monomorphiseType' err polys t2)
  (ForallT xs cxt ty0) -> do ty' <- monoGo err polys ty
                             case ty' of
                                  Nothing -> return ty0
                                  Just m  -> return m
  _                    -> return ty

-- From a set of constraints, find those which directly apply to a particular
-- name. For example, with `(Show a, Eq a, Eq b)` and `[a, b, c]` we'd get
-- `[(a, [Show, Eq]), (b, [Eq]), (c, [])]`
withConstraints :: Cxt -> [Name] -> [(Name, [Name])]
withConstraints cxt []     = []
withConstraints cxt (n:ns) = (n, go [] cxt) : withConstraints cxt ns
  where go acc [] = acc
        go acc ((AppT (ConT c) (VarT m)):xs) | n == m = go (c:acc) xs
        go acc (_:xs) = go acc xs

-- Given a class name, return all instances in scope
allInstancesOf :: Name -> Q [InstanceDec]
allInstancesOf n = do
  info <- reify n
  templateDebug (show info)
  case info of
       ClassI _ xs -> return xs
       _           -> error ("Reifying " ++ pprint n ++ " didn't give ClassI")

directInstancesOf :: Name -> Q [Name]
directInstancesOf c = do
    xs <- allInstancesOf c
    templateDebug ("directInstancesOf " ++ show c ++ ": " ++ show xs)
    return (getNames xs)
  where getNames ((InstanceD [] (AppT (ConT d) (ConT i))  _):xs) | d == c = i : getNames xs
        getNames (_:xs) = getNames xs
        getNames [] = []

-- Given a list of class names, return a list of type (constructors) which are
-- instances of all of those classes simultaneously
satisfyAll :: [Name] -> Q [Name]
satisfyAll []     = return [] -- We don't know the kind, so can't suggest anything
satisfyAll [c]    = directInstancesOf c
satisfyAll (c:cs) = do ns <- satisfyAll cs
                       ds <- directInstancesOf c
                       return (unify ds ns)
  where unify    []  ys               = []
        unify (x:xs) ys | x `elem` ys = x : unify xs ys
        unify (_:xs) ys               =     unify xs ys

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
    , (mkStr "type", mkStr (show (symbolType s)))
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
