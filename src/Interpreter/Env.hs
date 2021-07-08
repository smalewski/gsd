{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Interpreter.Env where

import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Map (Map, singleton, alter)
import qualified Data.Map.Strict as Map
import Interpreter.Syntax
import Interpreter.Type
import Control.Monad.Writer
import Data.List (foldl')
import Debug.Trace (trace)

newtype DataInfo = DataInfo
  { di_ctors :: [CtorName]
  }
  deriving (Eq, Show)

newtype CtorInfo = CtorInfo
  { argTypes ::  [(LabelName, Type)]
  }
  deriving (Eq, Show)

data Env a = Env
  { dataCtx :: Map DataName DataInfo,
    ctorCtx :: Map CtorName CtorInfo,
    varCtx :: Map Name a
  }
  deriving (Eq, Show, Functor)

newtype EnvM bot env acc err a = EnvM
  { runEnvM :: ReaderT (Env env) (WriterT acc  (ExceptT err bot)) a
  }
  deriving (Functor, Applicative, Monad, MonadReader (Env env), MonadWriter acc, MonadError err, MonadIO)

evalEnvM :: Env env -> EnvM Identity env acc err a -> Either err (a, acc)
evalEnvM env
  = runIdentity
  . runExceptT
  . runWriterT
  . flip runReaderT env
  . runEnvM

evalEnvMIO :: Env env -> EnvM IO env acc err a -> IO (Either err (a, acc))
evalEnvMIO env
  = runExceptT
  . runWriterT
  . flip runReaderT env
  . runEnvM


emptyEnv :: Env a
emptyEnv = Env mempty mempty mempty

-- Dump Contexts

dumpDataCtx :: (Monoid acc, Monad bot) => EnvM bot env acc err [(DataName, DataInfo)]
dumpDataCtx = asks (Map.assocs . dataCtx)

dumpCtorCtx :: (Monoid acc, Monad bot) => EnvM bot env acc err [(CtorName, CtorInfo)]
dumpCtorCtx = asks (Map.assocs . ctorCtx)

dumpVarCtx :: (Monoid acc, Monad bot) => EnvM bot env acc err [(Name, env)]
dumpVarCtx = asks (Map.assocs . varCtx)

-- Lookups

lookupData :: (Monoid acc, Monad bot) => DataName -> EnvM bot env acc err (Maybe DataInfo)
lookupData d = asks (lookupData' d)

lookupCtor :: (Monoid acc, Monad bot) => CtorName -> EnvM bot env acc err (Maybe CtorInfo)
lookupCtor c = asks (lookupCtor' c)

lookupVar :: (Monoid acc, Monad bot) => Name -> EnvM bot env acc err (Maybe env)
lookupVar x = asks (lookupVar' x)

lookupData' :: DataName -> Env a -> Maybe DataInfo
lookupData' d = Map.lookup d . dataCtx

lookupCtor' :: CtorName -> Env a -> Maybe CtorInfo
lookupCtor' c = Map.lookup c . ctorCtx

lookupVar' :: Name -> Env a -> Maybe a
lookupVar' x = Map.lookup x . varCtx

insertVar :: Env a -> Name -> a -> Env a
insertVar env x v =
  let newCtx = alter (const $ Just v) x $ varCtx env
  in Env (dataCtx env) (ctorCtx env) newCtx

insertCtor :: Env a -> CtorName -> CtorInfo -> Env a
insertCtor env c ci =
  let newCtx = alter (const $ Just ci) c $ ctorCtx env
  in Env (dataCtx env) newCtx (varCtx env)

extendData :: Env a -> DataName -> CtorName -> Env a
extendData env d c =
  let extendDataInfo (Just (DataInfo cs)) = Just $ DataInfo (c : cs)
      extendDataInfo Nothing              = Nothing
      newCtx = alter extendDataInfo d $ dataCtx env
  in Env newCtx (ctorCtx env) (varCtx env)

extendVarCtx :: Env a -> [(Name, a)] -> Env a
extendVarCtx env xvs =
  let f acc (x, v) = insertVar acc x v
  in foldl' f env xvs

lookupCtorLabel :: (Monoid acc, Monad bot) => LabelName -> CtorName -> EnvM bot env acc err (Maybe Type)
lookupCtorLabel l c = do
  cinfo <- lookupCtor c
  pure $ cinfo >>= lookup l . argTypes

err :: (Monoid acc, Monad bot) => err -> EnvM bot env acc err a
err = throwError

pass :: Monad m => m ()
pass = pure ()

openDatatypes :: (Monoid acc, Monad bot) => EnvM bot env acc err [DataName]
openDatatypes = asks (Map.keys . Map.filterWithKey (\k v -> isOpen k) . dataCtx)

datatypes :: (Monoid acc, Monad bot) => EnvM bot env acc err [DataName]
datatypes = asks (Map.keys . dataCtx)

ctorLabels :: (Monoid acc, Monad bot) => CtorName -> EnvM bot env acc err (Maybe [LabelName])
ctorLabels cname = do
  ctx <- asks ctorCtx
  let info = Map.lookup cname ctx
  pure $ map fst . argTypes <$> info

ctorTypes :: (Monoid acc, Monad bot) => CtorName -> EnvM bot env acc err (Maybe [Type])
ctorTypes cname = do
  ctx <- asks ctorCtx
  let info = Map.lookup cname ctx
  pure $ map snd . argTypes <$> info

withEnv :: (Monoid acc, Monad bot)
  => [(Name, env)]
  -> EnvM bot env acc err a
  -> EnvM bot env acc err a
withEnv mp comp = do
  ctx <- asks varCtx
  let newCtx = ctx <> Map.fromList mp
  local (\x -> x {varCtx = newCtx}) comp

errorMaybe :: (MonadError e m) => m a -> m (Maybe a)
errorMaybe ma = (Just <$> ma) `catchError` (\ _ -> pure Nothing)
