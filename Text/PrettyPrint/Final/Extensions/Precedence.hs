{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Text.PrettyPrint.Final.Extensions.Precedence
Description : A pretty printer extension for tracking precedence and associativity
Copyright   : (c) David Darais, David Christiansen, and Weixi Ma 2016-2017
License     : MIT
Maintainer  : david.darais@gmail.com
Stability   : experimental
Portability : Portable

A transformer of pretty monads that provides effects for inserting
parentheses minimally and correctly.
-}
module Text.PrettyPrint.Final.Extensions.Precedence
  ( -- * Precedence information
    PrecEnv(..)
  , precEnv0
    -- * Precedence effects
  , askLevel
  , localLevel
  , infl
  , infr
  , atLevel
  , botLevel
  , app
  , askBumped
  -- * The transformer
  , MonadReaderPrec(..)
  , MonadPrettyPrec(..)
  , PrecT(..)
  , runPrecT
  , mapPrecT
  ) where

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

import Text.PrettyPrint.Final as Final

-- | A precedence environment contains enough information to determine
-- whether parentheses should be inserted.
data PrecEnv ann = PrecEnv
  { level :: Int
    -- ^ The current precedence level of the context
  , bumped :: Bool
    -- ^ A tiebreaker used to distinguish left- and right-associative contexts
  , lparen :: (Text, Maybe ann)
    -- ^ What to show for a left parenthesis. The optional annotation
    -- will be applied.
  , rparen :: (Text, Maybe ann)
    -- ^ What to show for a right parenthesis. The optional annotation
    -- will be applied.
  }

-- | An initial precedence environment that works for languages with
-- parentheses as delimiters.
precEnv0 :: PrecEnv ann
precEnv0 = PrecEnv
  { level = 0
  , bumped = False
  , lparen = ("(", Nothing)
  , rparen = (")", Nothing)
  }


-- | Precedence follows the structure of a document, so a Reader
-- provides the appropriate dynamic extent of precedence information.
class MonadReaderPrec ann m | m -> ann where
  -- | What is the current precedence environment? (see 'ask')
  askPrecEnv :: m (PrecEnv ann)
  -- | Override the precedence environment in a subcomputation. (see 'local')
  localPrecEnv :: (PrecEnv ann -> PrecEnv ann) -> m a -> m a

-- | What is the current precedence level?
askLevel :: (Functor m, MonadReaderPrec ann m) => m Int
askLevel = level <$> askPrecEnv

-- | Run a subcomputation with a modified precedence level.
localLevel :: (Functor m, MonadReaderPrec ann m) => (Int -> Int) -> m a -> m a
localLevel f = localPrecEnv $ \ pe -> pe { level = f $ level pe }

-- | Is the current precedence bumped? See 'PrecEnv'.
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

-- | A pretty monad that can read precedence environments
class ( MonadPretty w ann fmt m
      , MonadReaderPrec ann m
      ) =>
      MonadPrettyPrec w ann fmt m
      | m -> w, m -> ann, m -> fmt

-- Operations

-- | Put a subdocument in the lowest precedence context
botLevel :: (MonadPrettyPrec w ann fmt m) => m () -> m ()
botLevel = localLevel (const 0) . localBumped (const False)

-- | Close a context with left and right delimiters
closed :: (MonadPrettyPrec w ann fmt m) => m () -> m () -> m () -> m ()
closed alM arM aM = do
  alM
  botLevel $ aM
  arM

-- | Close a context with the configured left and right parentheses
parens :: (MonadPrettyPrec w ann fmt m) => m () -> m ()
parens aM = do
  (lp, lpA) <- askLParen
  (rp, rpA) <- askRParen
  let lpD = maybe id annotate lpA $ text lp
      rpD = maybe id annotate rpA $ text rp
  closed lpD rpD $ align aM

-- | Run a subcomputation at a particular precedence level
atLevel :: (MonadPrettyPrec w ann fmt m) => Int -> m () -> m ()
atLevel i' aM = do
  i <- askLevel
  b <- askBumped
  let aM' = localLevel (const i') $ localBumped (const False) aM
  if i < i' || (i == i' && not b)
    then aM'
    else parens aM'

-- | Bump the precedence to implement associativity
bump :: (MonadPrettyPrec w ann fmt m) => m a -> m a
bump = localBumped $ const True

-- | Display a non-associative infix operator at a precedence level
inf :: (MonadPrettyPrec w ann fmt m) => Int -> m () -> m () -> m () -> m ()
inf i oM x1M x2M = atLevel i $ bump x1M >> space 1 >> oM >> space 1 >> bump x2M

-- | Display a left-associative infix operator at a precedence level
infl :: (MonadPrettyPrec w ann fmt m) => Int -> m () -> m () -> m () -> m ()
infl i oM x1M x2M = atLevel i $ x1M >> space 1 >> oM >> space 1 >> bump x2M

-- | Display a right-associative infix operator at a precedence level
infr :: (MonadPrettyPrec w ann fmt m) => Int -> m () -> m () -> m () -> m ()
infr i oM x1M x2M = atLevel i $ bump x1M >> space 1 >> oM >> space 1 >> x2M

-- | Display a prefix operator at a precedence level
pre :: (MonadPrettyPrec w ann fmt m) => Int -> m () -> m () -> m ()
pre i oM xM = atLevel i $ oM >> space 1 >> xM

-- | Display a postfix operator at a precedence level
post :: (MonadPrettyPrec w ann fmt m) => Int -> m () -> m () -> m ()
post i oM xM = atLevel i $ xM >> space 1 >> oM

-- | Perform function application with precedence level <100
app :: (MonadPrettyPrec w ann fmt m) => m () -> [m ()] -> m ()
app x xs = atLevel 100 $ hvsep $ x : map (align . bump) xs

-- | Lay out a collection like 'Final.collection', but reset the precedence level.
collection :: (MonadPrettyPrec w ann fmt m) => m () -> m () -> m () -> [m ()] -> m ()
collection open close sep = Final.collection open close sep . map botLevel

-- Monad Transformer
-- | A monad transformer that adds a precedence effects
newtype PrecT ann m a = PrecT { unPrecT :: ReaderT (PrecEnv ann) m a }
  deriving
  ( Functor, Monad, Applicative, Alternative, MonadTrans
  , MonadState s, MonadWriter o
  )

-- | Run a precedence transformer with some initial precedence environment
runPrecT :: PrecEnv ann -> PrecT ann m a -> m a
runPrecT pr xM = runReaderT (unPrecT xM) pr

-- | Transform the value returned by a 'PrecT'
mapPrecT :: (m a -> n b) -> PrecT ann m a -> PrecT ann n b
mapPrecT f = PrecT . mapReaderT f . unPrecT

instance (MonadReader r m) => MonadReader r (PrecT ann m) where
  ask = PrecT $ lift ask
  local f = PrecT . mapReaderT (local f) . unPrecT

instance (Monad m) => MonadReaderPrec ann (PrecT ann m) where
  askPrecEnv = PrecT ask
  localPrecEnv f = PrecT . local f . unPrecT
