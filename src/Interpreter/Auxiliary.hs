module Interpreter.Auxiliary where

import Prelude hiding (span)

import Control.Applicative (liftA2)
import Control.Monad.Reader
import Data.List (find, sort)
import Interpreter.Env
import Interpreter.Error
import Interpreter.Span (Span, span)
import Interpreter.Syntax.Common
import Interpreter.Type
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import Data.Maybe (isNothing, fromMaybe, catMaybes)
import Debug.Trace (traceShowM, traceShow)


-- * Consistent lifting of functions

-- | Domain
dom :: (IsError err, Monoid acc, Monad bot) => Span -> Type -> EnvM bot env acc err Type
dom _ (TArr _ t _) = pure t
dom _ t@(TUnkn _)  = pure t
dom sp t = err $ errConsistency sp t (TArr sp (TUnkn sp) (TUnkn sp))

-- | Domain
cod :: (IsError err, Monoid acc, Monad bot) => Span -> Type -> EnvM bot env acc err Type
cod _ (TArr _ _ t) = pure t
cod _ t@(TUnkn _) = pure t
cod sp t = err $ errConsistency sp t (TArr sp (TUnkn sp) (TUnkn sp))

ctors :: (IsError err, Monoid acc, Monad bot) =>  Span -> DataName -> EnvM bot env acc err [CtorName]
ctors sp d = lookupData d >>= maybe (err $ errDataNotFound sp d) (pure . di_ctors)

cty :: (IsError err, Monoid acc, Monad bot) => Span -> CtorName -> EnvM bot env acc err Type
cty sp c = do
  delta <- dumpDataCtx
  let md = fst <$> find (\(_, dinfo) -> c `elem` di_ctors dinfo) delta
      openDs = filter isOpen $ fst <$> delta
  case (md, openDs) of
    (Just d, _)    -> pure $ TData sp d
    (Nothing, [])  -> err  $ errCtorNotFound sp c
    (Nothing, [d]) -> pure $ TData sp d
    (Nothing, _)   -> pure $ TUnclass sp

fty :: (IsError err, Monoid acc, Monad bot) => Span -> LabelName -> Type -> EnvM bot env acc err Type
fty sp l dt@(TData _ d)
  | isOpen d = ifM (hasLabel sp d l)
                 thenM meetLty
                 elseM (pure $ TUnkn sp)
  |otherwise = meetLty
  where
  meetLty  = do
    ltypes <- catMaybes <$> (ctors sp d >>= mapM (errorMaybe . lty sp l))
    meetLty' <- ifEmpty (err $ errNoLabel sp l dt) (pure . meetList) ltypes
    maybe (err $ errLabelNotConsistent sp l dt) pure meetLty'
fty sp l (TUnclass sp') = joinFty sp sp' l openDatatypes
fty sp l (TUnknData sp') = joinFty sp sp' l datatypes
fty sp l (TUnkn sp') = fty sp l (TUnknData sp')
fty sp _ t = err $ errConsistency sp t (TUnknData sp)

ifEmpty :: b -> ([a] -> b) -> [a] -> b
ifEmpty z _ [] = z
ifEmpty _ f xs = f xs

joinFty :: (IsError err, Monoid acc, Monad bot)
        => Span -> Span -> LabelName
        -> EnvM bot env acc err [DataName]
        -> EnvM bot env acc err Type
joinFty sp sp' l datas = joinList . catMaybes <$> (datas >>= mapM (errorMaybe . fty sp l . TData sp'))

lty :: (IsError err, Monoid acc, Monad bot) => Span -> LabelName -> CtorName -> EnvM bot env acc err Type
lty sp l c =
  ifM (isUnclass c)
    thenM (pure $ TUnkn sp)
    elseM (fromMaybeM (err $ errNoCtorLabel sp l c) (lookupCtorLabel l c))

parg :: (IsError err, Monoid acc, Monad bot)
     => Pattern
     -> EnvM bot env acc err [(Name, Type)]
parg (CtorP sp c xs) = do
  ts <- ifM (isUnclass c)
      thenM (pure . replicate (length xs) $ TUnkn mempty)
      elseM (fromMaybeM (pure []) (ctorTypes c))
  when (length xs /= length ts) (err $ errInvalidLabels sp c)
  pure $ zip xs ts
parg (DefP _) = pure []

equate :: [Type] -> Maybe Type
equate = meetList

-- * Consistent lifting of predicates

isUnclass :: (IsError err, Monoid acc, Monad bot) => CtorName -> EnvM bot env acc err Bool
isUnclass c = isNothing <$> lookupCtor c

singleton :: a -> [a]
singleton x = [x]

-- | Validity of match expression
valid :: (IsError err, Monoid acc, Monad bot) => Valid -> [Pattern] -> Type -> EnvM bot env acc err ()
valid Exact ps t
  | hasDefP ps = valid Sound ps t

valid Complete ps _
  | hasDefP ps = pass

-- Exact without default case, or Sound
valid v ps t = do
  cssTy <- ctorsPerType t
  (uncs, cs) <- partitionM isUnclass $ concatMap pctor ps
  traceShowM (uncs, cs)
  traceShowM cssTy
  let csP = S.fromList cs
      isValid = case v of
            Sound    -> any (csP `S.isSubsetOf`) cssTy
            Exact    -> csP `elem` cssTy
            Complete -> any (`S.isSubsetOf` csP) cssTy
      noUnclass = null uncs
  traceShowM (isValid, noUnclass)
  if isValid && validUnclass v noUnclass (isOpenType t)
    then pass
    else err $ errInvalidMatch mempty v t

validUnclass :: Valid
             -> Bool -- There are no unclass patterns
             -> Bool -- Is Open?
             -> Bool
validUnclass Complete _     _     = True
validUnclass _        _     True  = True
validUnclass _        True  False = True
validUnclass _        False False = False

isOpenType :: Type -> Bool
isOpenType (TData _ d) = isOpen d
isOpenType t           = isUnkn t

ctorsPerType :: (IsError err, Monoid acc, Monad bot)
             => Type
             -> EnvM bot env acc err [Set CtorName]
ctorsPerType (TData _ d) = maybe [] (singleton . S.fromList . di_ctors) <$> lookupData d
ctorsPerType (TUnclass _) = asks (fmap (S.fromList . di_ctors) . Map.elems . Map.filterWithKey (\k _ -> isOpen k) . dataCtx)
ctorsPerType (TUnknData _) = asks (fmap (S.fromList . di_ctors) . Map.elems . dataCtx)
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
  ls' <- ctorLabels c >>= maybe (pure ls) pure
  if sort ls == sort ls'
    then pure ()
    else err $ errInvalidLabels sp c

hasLabel :: (IsError err, Monoid acc, Monad bot) => Span -> DataName -> LabelName -> EnvM bot env acc err Bool
hasLabel _ d l = do
  dinfo <- lookupData d
  ctorsMayLs <- mapM ctorLabels (maybe [] di_ctors dinfo)
  let ctorsLs = fromMaybe [] <$> ctorsMayLs
  pure $ any (elem l) ctorsLs

-- Utils

mapMaybeM :: Applicative m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = foldr g (pure [])
  where
    g a = liftA2 (maybe id (:)) (f a)

cantFail :: (IsError err, Monoid acc, Monad bot) => EnvM bot env acc err (Maybe a) -> EnvM bot env acc err a
cantFail m = m >>= maybe (err errImposible) pure

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = pure ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    pure ([x | res]++as, [x | not res]++bs)

ifM :: Monad m => m Bool -> () -> m a -> () -> m a -> m a
ifM b _ t _ f = do b' <- b; if b' then t else f

thenM :: ()
thenM = ()

elseM :: ()
elseM = ()
-- | Monadic generalisation of 'maybe'.
maybeM :: Monad m => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM n j x = maybe n j =<< x

-- | Monadic generalisation of 'fromMaybe'.
fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM n = maybeM n pure
