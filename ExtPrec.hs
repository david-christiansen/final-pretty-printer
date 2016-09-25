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

module ExtPrec where

import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Data.List
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T

import Pretty

env0 :: (Num w) => PEnv w ann ()
env0 = PEnv
  { maxWidth = 80
  , maxRibbon = 60
  , layout = Break
  , failure = CantFail
  , nesting = 0
  , formatting = mempty
  , formatAnn = const mempty
  }

state0 :: PState Int ()
state0 = PState
  { curLine = []
  }

data PrecEnv = PrecEnv
  { level :: Int
  , bumped :: Bool
  }

precEnv0 :: PrecEnv
precEnv0 = PrecEnv
  { level = 0
  , bumped = False
  }

class MonadReaderPrec m where
  askPrecEnv :: m PrecEnv
  localPrecEnv :: (PrecEnv -> PrecEnv) -> m a -> m a

askLevel :: (Functor m, MonadReaderPrec m) => m Int
askLevel = level <$> askPrecEnv

localLevel :: (Functor m, MonadReaderPrec m) => (Int -> Int) -> m a -> m a
localLevel f = localPrecEnv $ \ pe -> pe { level = f $ level pe }

askBumped :: (Functor m, MonadReaderPrec m) => m Bool
askBumped = bumped <$> askPrecEnv

localBumped :: (Functor m, MonadReaderPrec m) => (Bool -> Bool) -> m a -> m a
localBumped f = localPrecEnv $ \ pe -> pe { bumped = f $ bumped pe }

class (MonadPretty w ann fmt m, MonadReaderPrec m) => MonadPrettyPrec w ann fmt m | m -> w, m -> ann, m -> fmt

-- Operations

botLevel :: (MonadPrettyPrec w ann fmt m) => m () -> m ()
botLevel = localLevel (const 0) . localBumped (const False)

closed :: (MonadPrettyPrec w ann fmt m) => m () -> m () -> m () -> m ()
closed alM arM aM = do
  alM
  botLevel $ aM
  arM

parens :: (MonadPrettyPrec w ann fmt m) => m () -> m ()
parens = closed (text "(") (text ")") . align

atLevel :: (MonadPrettyPrec w ann fmt m) => Int -> m () -> m ()
atLevel i' aM = do
  i <- askLevel
  b <- askBumped
  if i < i' || (i == i' && not b)
    then localLevel (const i') $ localBumped (const False) aM
    else parens aM

bump :: (MonadPrettyPrec w ann fmt m) => m a -> m a
bump = localBumped $ const True

inf :: (MonadPrettyPrec w ann fmt m) => Int -> m () -> m () -> m () -> m ()
inf i oM x1M x2M = atLevel i $ bump x1M >> space 1 >> oM >> space 1 >> bump x2M

infl :: (MonadPrettyPrec w ann fmt m) => Int -> m () -> m () -> m () -> m ()
infl i oM x1M x2M = atLevel i $ x1M >> space 1 >> oM >> space 1 >> bump x2M

infr :: (MonadPrettyPrec w ann fmt m) => Int -> m () -> m () -> m () -> m ()
infr i oM x1M x2M = atLevel i $ bump x1M >> space 1 >> oM >> space 1 >> x2M

pre :: (MonadPrettyPrec w ann fmt m) => Int -> m () -> m () -> m ()
pre i oM xM = atLevel i $ oM >> space 1 >> xM

post :: (MonadPrettyPrec w ann fmt m) => Int -> m () -> m () -> m ()
post i oM xM = atLevel i $ xM >> space 1 >> oM

app :: (MonadPrettyPrec w ann fmt m) => m () -> [m ()] -> m ()
app x xs = atLevel 100 $ hvsep $ x : map (align . bump) xs

collection :: (MonadPrettyPrec w ann fmt m) => m () -> m () -> m () -> [m ()] -> m ()
collection open close _   []     = open >> close
collection open close sep (x:xs) = grouped $ hvsepTight $ concat
  [ pure $ hsepTight [open, botLevel $ align x]
  , flip map xs $ \ x' -> hsep [sep, botLevel $ align x']
  , pure $ close
  ]
