{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Text.PrettyPrint.Final.Extensions.Environment
Description : Lexical scope tracking for final pretty printers
Copyright   : (c) David Darais, David Christiansen, and Weixi Ma 2016-2017
License     : MIT
Maintainer  : david.darais@gmail.com
Stability   : experimental
Portability : Portable

'EnvT' extends a pretty printer to track a lexical environment.
-}

module Text.PrettyPrint.Final.Extensions.Environment
  ( MonadPrettyEnv(..)
  , MonadReaderEnv(..)
  -- * The transformer
  , EnvT(..)
  , runEnvT
  , mapEnvT
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer


import Text.PrettyPrint.Final as Final
import Text.PrettyPrint.Final.Extensions.Precedence

-- | A reader of environments
class MonadReaderEnv env m | m -> env where
  -- | See 'ask'
  askEnv :: m env
  -- | See 'local'
  localEnv :: (env -> env) -> m a -> m a

-- | Pretty monads that can read environments. Use this to implement
-- lexical scope in your pretty printer, because the dynamic extent of
-- pretty monad computations typically corresponds to the scope of a
-- binder.
class ( MonadPretty w ann fmt m
      , MonadReaderEnv env m
      ) => MonadPrettyEnv env w ann fmt m
      | m -> w, m -> ann, m -> fmt, m -> env where

-- | A transformer that adds a reader effect, distinguished by the newtype tag.
newtype EnvT env m a = EnvT { unEnvT :: ReaderT env m a }
  deriving
    ( Functor, Monad, Applicative, Alternative, MonadTrans
    , MonadState s, MonadWriter o
    )

-- | Run a pretty printer in an initial environment
runEnvT :: env -> EnvT env m a -> m a
runEnvT e xM = runReaderT (unEnvT xM) e

-- | Transform the result of a pretty printer
mapEnvT :: (m a -> n b) -> EnvT env m a -> EnvT env n b
mapEnvT f = EnvT . mapReaderT f . unEnvT

instance MonadReader r m => MonadReader r (EnvT env m) where
  ask = EnvT $ lift ask
  local f = mapEnvT (local f)

instance (Monad m, Measure w fmt m) => Measure w fmt (EnvT env m) where
  measure = lift . measure

instance MonadPretty w ann fmt m => MonadPretty w ann fmt (EnvT env m) where

instance Monad m => MonadReaderEnv env (EnvT env m) where
  askEnv = EnvT ask
  localEnv f = EnvT . local f . unEnvT

instance (Monad m, MonadReaderPrec ann m) => MonadReaderPrec ann (EnvT env m) where
  askPrecEnv = lift askPrecEnv
  localPrecEnv f (EnvT (ReaderT x)) = EnvT (ReaderT (\env -> localPrecEnv f (x env)))
