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

module Exts.ExtPrec where

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

-- Environment for Prec extension

data PrecEnv ann = PrecEnv
  { level :: Int
  , bumped :: Bool
  , lparen :: (Text, Maybe ann)
  , rparen :: (Text, Maybe ann)
  }

precEnv0 :: PrecEnv ann
precEnv0 = PrecEnv
  { level = 0
  , bumped = False
  , lparen = ("(", Nothing)
  , rparen = (")", Nothing)
  }

-- Type class for Prec extension

class MonadReaderPrec ann m | m -> ann where
  askPrecEnv :: m (PrecEnv ann)
  localPrecEnv :: (PrecEnv ann -> PrecEnv ann) -> m a -> m a

askLevel :: (Functor m, MonadReaderPrec ann m) => m Int
askLevel = level <$> askPrecEnv

localLevel :: (Functor m, MonadReaderPrec ann m) => (Int -> Int) -> m a -> m a
localLevel f = localPrecEnv $ \ pe -> pe { level = f $ level pe }

askBumped :: (Functor m, MonadReaderPrec ann m) => m Bool
askBumped = bumped <$> askPrecEnv

localBumped :: (Functor m, MonadReaderPrec ann m) => (Bool -> Bool) -> m a -> m a
localBumped f = localPrecEnv $ \ pe -> pe { bumped = f $ bumped pe }

askLParen :: (Functor m, MonadReaderPrec ann m) => m (Text, Maybe ann)
askLParen = lparen <$> askPrecEnv

localLParen :: (Functor m, MonadReaderPrec ann m) => ((Text, Maybe ann) -> (Text, Maybe ann)) -> m a -> m a
localLParen f = localPrecEnv $ \ pe -> pe { lparen = f $ lparen pe }

askRParen :: (Functor m, MonadReaderPrec ann m) => m (Text, Maybe ann)
askRParen = rparen <$> askPrecEnv

localRParen :: (Functor m, MonadReaderPrec ann m) => ((Text, Maybe ann) -> (Text, Maybe ann)) -> m a -> m a
localRParen f = localPrecEnv $ \ pe -> pe { rparen = f $ rparen pe }

class (MonadPretty w ann fmt m, MonadReaderPrec ann m) => MonadPrettyPrec w ann fmt m | m -> w, m -> ann, m -> fmt

-- Operations

botLevel :: (MonadPrettyPrec w ann fmt m) => m () -> m ()
botLevel = localLevel (const 0) . localBumped (const False)

closed :: (MonadPrettyPrec w ann fmt m) => m () -> m () -> m () -> m ()
closed alM arM aM = do
  alM
  botLevel $ aM
  arM

parens :: (MonadPrettyPrec w ann fmt m) => m () -> m ()
parens aM = do
  (lp, lpA) <- askLParen
  (rp, rpA) <- askRParen 
  let lpD = maybe id annotate lpA $ text lp 
      rpD = maybe id annotate rpA $ text rp
  closed lpD rpD $ align aM

atLevel :: (MonadPrettyPrec w ann fmt m) => Int -> m () -> m ()
atLevel i' aM = do
  i <- askLevel
  b <- askBumped
  let aM' = localLevel (const i') $ localBumped (const False) aM
  if i < i' || (i == i' && not b)
    then aM'
    else parens aM'

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
collection open close sep = Pretty.collection open close sep . map botLevel

-- Monad Transformer

newtype PrecT ann m a = PrecT { unPrecT :: ReaderT (PrecEnv ann) m a }
  deriving
  ( Functor, Monad, Applicative, Alternative, MonadTrans
  , MonadState s, MonadWriter o
  )

runPrecT :: PrecEnv ann -> PrecT ann m a -> m a
runPrecT pr xM = runReaderT (unPrecT xM) pr

mapPrecT :: (m a -> n b) -> PrecT ann m a -> PrecT ann n b
mapPrecT f = PrecT . mapReaderT f . unPrecT

instance (MonadReader r m) => MonadReader r (PrecT ann m) where
  ask = PrecT $ lift ask
  local f = PrecT . mapReaderT (local f) . unPrecT
instance (Monad m) => MonadReaderPrec ann (PrecT ann m) where
  askPrecEnv = PrecT ask
  localPrecEnv f = PrecT . local f . unPrecT
