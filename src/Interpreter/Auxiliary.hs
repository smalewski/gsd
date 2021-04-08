module Interpreter.Auxiliary where

import Prelude hiding (span)

import Control.Applicative (liftA2)
import Control.Monad (foldM)
import Control.Monad.Reader
import Data.List (find, sort)
import Interpreter.Env
import Interpreter.Error
import Interpreter.Span (Span, span)
import Interpreter.Syntax.Common
import Interpreter.Type
import Debug.Trace (trace)
import Data.Set (Set)
import qualified Data.Set as S


-- * Consistent lifting of functions

-- | Domain
dom :: (IsError err, Monoid acc, Monad bot) => Span -> Type -> EnvM bot env acc err Type
dom _ (TArr _ t _) = pure t
dom _ t@(TUnkn _) = pure t
dom sp t = err $ errConsistency sp t (TArr sp (TUnkn sp) (TUnkn sp))

-- | Domain
cod :: (IsError err, Monoid acc, Monad bot) => Span -> Type -> EnvM bot env acc err Type
cod _ (TArr _ _ t) = pure t
cod _ t@(TUnkn _) = pure t
cod sp t = err $ errConsistency sp t (TArr sp (TUnkn sp) (TUnkn sp))

ctors :: (IsError err, Monoid acc, Monad bot) =>  Span -> DataName -> EnvM bot env acc err [CtorName]
ctors sp d = lookupData (Just d) >>= maybe (err $ errDataNotFound sp d) (pure . di_ctors)

cty :: (IsError err, Monoid acc, Monad bot) => Span -> CtorName -> EnvM bot env acc err Type
cty sp c = do
  delta <- dumpDataCtx
  let md = fst <$> find (\(_, dinfo) -> c `elem` di_ctors dinfo) delta
  d <- maybe (err $ errCtorNotFound sp c) pure md
  pure $ maybe (TUnclass mempty) (TData mempty) d

fty :: (IsError err, Monoid acc, Monad bot) => Span -> LabelName -> Type -> EnvM bot env acc err Type
fty sp l t = do
  cs <- S.toList . S.unions <$> ctorsPerType t
  ts <- mapMaybeM (lookupCtorLabel l) cs
  ts' <- case t of
           TData _ d
             | null ts -> do b <- isOpen d
                             if b
                               then pure [TUnkn mempty]
                               else err $ errNoLabel sp l t
             | otherwise -> pure ts
           _ -> pure $ if null ts then [TUnkn mempty] else ts
  let error = err $ errLabelNotConsistent sp l t
  equate ts' >>= maybe error pure

lty :: (IsError err, Monoid acc, Monad bot) => Span -> LabelName -> CtorName -> EnvM bot env acc err Type
lty sp l c = lookupCtorLabel l c >>= maybe (err $ errCtorNotFound sp c) pure

parg :: (IsError err, Monoid acc, Monad bot)
     => Pattern
     -> EnvM bot env acc err [(Name, Type)]
parg (CtorP sp c xs) = do
  types <- ctorTypes c >>= maybe (err $ errCtorNotFound sp c) pure
  pure $ zip xs types
--parg (ArityP sp name xs) = pure $ (name, TBase mempty TString) : zip xs (repeat $ TUnkn mempty)
parg (DefP sp') = pure []


-- * Consistent lifting of predicates

-- | Validity of match expression
valid :: (IsError err, Monoid acc, Monad bot) => Valid -> [Pattern] -> Type -> EnvM bot env acc err ()
valid Exact ps t
  | hasDefP ps = valid Sound ps t

valid Complete ps t
  | hasDefP ps = pass

-- Exact without default case, or Sound
valid v ps t = do
  cssTy <- ctorsPerType t
  let csP = S.fromList $ concatMap pctor ps
      isValid = case v of
            Sound    -> any (csP `S.isSubsetOf`) cssTy
            Exact    -> csP `elem` cssTy
            Complete -> any (`S.isSubsetOf` csP) cssTy
  if isValid
    then pass
    else err $ errInvalidMatch mempty v t

ctorsPerType :: (IsError err, Monoid acc, Monad bot)
             => Type
             -> EnvM bot env acc err [Set CtorName]
ctorsPerType (TData sp d) = do
  DataInfo o cs <- lookupData (Just d) >>= maybe (err $ errDataNotFound sp d) pure
  extras <- case o of
             Closed -> pure mempty
             Open   -> lookupData Nothing >>= maybe (pure mempty) (pure . di_ctors)
  pure [S.fromList $ cs <> extras]

ctorsPerType (TUnclass sp) = do
  dump <- dumpDataCtx
  let isOpenData (Just _, DataInfo Open _) = True
      isOpenData _                         = False
      css = S.fromList . di_ctors . snd <$> filter isOpenData dump
      extras = maybe mempty (S.fromList . di_ctors) $ lookup Nothing dump
  pure (fmap (<> extras) css)

ctorsPerType (TUnknData sp) = do
  dump <- dumpDataCtx
  let isOpenData (Just _, DataInfo Open _) = True
      isOpenData _                         = False
      isClosedData (Just _, DataInfo Closed _) = True
      isClosedData _                           = False
      opens = S.fromList . di_ctors . snd <$> filter isOpenData dump
      closeds = S.fromList . di_ctors . snd <$> filter isClosedData dump
      extras = maybe mempty (S.fromList . di_ctors) $ lookup Nothing dump
  pure $ closeds <> fmap (<> extras) opens

ctorsPerType (TUnkn sp) = ctorsPerType (TUnknData sp)
ctorsPerType t = err $ errConsistency (span t) t (TUnknData mempty)

hasDefP :: [Pattern] -> Bool
hasDefP []            = False
hasDefP (DefP _ : _) = True
hasDefP (_ : ps)     = hasDefP ps

pctor :: Pattern -> [CtorName]
pctor (CtorP _ c _) = [c]
pctor (DefP  _)     = []

satisfyLabels :: (IsError err, Monoid acc, Monad bot) => Span -> CtorName -> [LabelName] -> EnvM bot env acc err ()
satisfyLabels sp c ls = do
  ls' <- ctorLabels c >>= maybe (err $ errCtorNotFound sp c) pure
  if sort ls == sort ls'
    then pure ()
    else err $ errInvalidLabels sp c

isOpen :: (Monoid acc, Monad bot) => DataName -> EnvM bot env acc err Bool
isOpen d = asks (isOpen' d)

precise :: (Monoid acc, Monad bot) => Type -> Type -> EnvM bot env acc err Bool
precise t1 t2 = asks (precise' t1 t2)

meet :: (Monoid acc, Monad bot) => Type -> Type -> EnvM bot env acc err (Maybe Type)
meet t1 t2 = asks (meet' t1 t2)

equate :: (Monoid acc, Monad bot) => [Type] -> EnvM bot env acc err (Maybe Type)
equate ts = asks (equate' ts)

-- Pure versions

isOpen' :: DataName -> Env a -> Bool
isOpen' d env = (Just Open ==) $ di_openess <$> lookupData' (Just d) env

precise' :: Type -> Type -> Env a -> Bool
precise' t1 t2 _ | t1 == t2 = True
precise' _ (TUnkn _) _ = True
precise' (TUnclass _) (TUnknData _) _ = True
precise' (TData _ _) (TUnknData _) _ = True
precise' (TData _ d) (TUnclass _) env = isOpen' d env
precise' _ _ _ = False

meet' :: Type -> Type -> Env a -> Maybe Type
meet' (TArr sp1 t11 t12) (TArr sp2 t21 t22) env =
  TArr (sp1 <> sp2) <$> meet' t11 t21 env <*> meet' t12 t22 env
meet' t1 t2 env
  | precise' t1 t2 env = Just t1
  | precise' t2 t1 env = Just t2
  | otherwise = Nothing

equate' :: [Type] -> Env a -> Maybe Type
equate' [] _ = Nothing
equate' ts env = foldM (\t1 t2 -> meet' t1 t2 env) (TUnkn mempty) ts

-- Utils

mapMaybeM :: Applicative m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = foldr g (pure [])
  where
    g a = liftA2 (maybe id (:)) (f a)

cantFail :: (IsError err, Monoid acc, Monad bot) => EnvM bot env acc err (Maybe a) -> EnvM bot env acc err a
cantFail m = m >>= maybe (err errImposible) pure
