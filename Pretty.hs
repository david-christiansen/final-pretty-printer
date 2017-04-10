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

module Pretty where

import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

data Chunk w = 
    CText Text -- should not contain formatting spaces or newlines
               -- (semantic/object-level spaces OK, but not newlines)
  | CSpace w
  deriving (Eq, Ord)

data Atom w = AChunk (Chunk w) | ANewline
  deriving (Eq, Ord)

type Line w fmt = [(Chunk w, fmt)]

data POut w ann =
    PNull
  | PAtom (Atom w)
  | PAnn ann (POut w ann)
  | PSeq (POut w ann) (POut w ann)
  deriving (Eq, Ord, Functor)

instance Monoid (POut w ann) where
  mempty = PNull
  mappend = PSeq

class Measure w fmt m | m -> w, m -> fmt where
  measure :: Line w fmt -> m w

data Identity a = Identity { runIdentity :: a }
  deriving (Functor)
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity $ f x

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

instance Measure Int () Identity where
  measure = pure . sum . fmap (chunkLength . fst)
    where
      chunkLength (CText t) = T.length t
      chunkLength (CSpace w) = w

class
  ( Ord w, Num w
  , Monoid fmt
  , Measure w fmt m
  , Monad m
  , MonadReader (PEnv w ann fmt) m
  , MonadWriter (POut w ann) m
  , MonadState (PState w fmt) m
  , Alternative m
  ) => MonadPretty (w :: *) (ann :: *) (fmt :: *) (m :: * -> *) | m -> w, m -> ann, m -> fmt where

data Layout = Flat | Break
  deriving (Eq, Ord)
data Failure = CanFail | CantFail
  deriving (Eq, Ord)

data PEnv w ann fmt = PEnv
  { maxWidth :: w
  , maxRibbon :: w
  , nesting :: w
  , layout :: Layout
  , failure :: Failure
  , formatting :: fmt -- ^ A stack of formatting codes to be combined with the monoid op
  , formatAnn :: ann -> fmt
  }

askMaxWidth :: (MonadReader (PEnv w ann fmt) m) => m w
askMaxWidth = maxWidth <$> ask

localMaxWidth :: (MonadReader (PEnv w ann fmt) m) => (w -> w) -> m a -> m a
localMaxWidth f = local $ \ r -> r { maxWidth = f (maxWidth r) }

askMaxRibbon :: (MonadReader (PEnv w ann fmt) m) => m w
askMaxRibbon = maxRibbon <$> ask

askNesting :: (MonadReader (PEnv w ann fmt) m) => m w
askNesting = nesting <$> ask

localNesting :: (MonadReader (PEnv w ann fmt) m) => (w -> w) -> m a -> m a
localNesting f = local $ \ r -> r { nesting = f (nesting r) }

askFormat :: (MonadReader (PEnv w ann fmt) m, Monoid fmt) => m fmt
askFormat = formatting <$> ask

localFormat :: (Monoid fmt, MonadReader (PEnv w ann fmt) m) => (fmt -> fmt) -> m a -> m a
localFormat f = local $ \ r -> r { formatting = f (formatting r) }

pushFormat :: (Monoid fmt, MonadReader (PEnv w ann fmt) m) => fmt -> m a -> m a
pushFormat format = localFormat (flip mappend format )

askFormatAnn :: (MonadReader (PEnv w ann fmt) m, Monoid fmt) => m (ann -> fmt)
askFormatAnn = formatAnn <$> ask

localFormatAnn :: (MonadReader (PEnv w ann fmt) m) => ((ann -> fmt) -> (ann -> fmt)) -> m a -> m a
localFormatAnn f = local $ \ r -> r { formatAnn = f (formatAnn r) }

askLayout :: (MonadReader (PEnv w ann fmt) m) => m Layout
askLayout = layout <$> ask

localLayout :: (MonadReader (PEnv w ann fmt) m) => (Layout -> Layout) -> m a -> m a
localLayout f = local $ \ r -> r { layout = f (layout r) }

askFailure :: (MonadReader (PEnv w ann fmt) m) => m Failure
askFailure = failure <$> ask

localFailure :: (MonadReader (PEnv w ann fmt) m) => (Failure -> Failure) -> m a -> m a
localFailure f = local $ \ r -> r { failure = f (failure r) }

data PState w fmt = PState
  { curLine :: Line w fmt
  }
  deriving (Eq, Ord)

getCurLine :: (MonadState (PState w fmt) m) => m (Line w fmt)
getCurLine = curLine <$> get

putCurLine :: (MonadState (PState w fmt) m) => Line w fmt -> m ()
putCurLine t = modify $ \ s -> s { curLine = t }

measureCurLine :: (Measure w fmt m, Monad m, MonadState (PState w fmt) m) => m w
measureCurLine = measure =<< getCurLine

modifyLine :: (MonadState (PState w fmt) m) => (Line w fmt -> Line w fmt) -> m ()
modifyLine f = modify $ \ s -> s { curLine = f (curLine s) }

-- not exported
-- this is the core algorithm.
chunk :: (MonadPretty w ann fmt m) => Chunk w -> m ()
chunk c = do
  tell $ PAtom $ AChunk c
  format <- askFormat
  modifyLine $ flip mappend [(c, format)]
  f <- askFailure
  when (f == CanFail) $ do
    wmax <- askMaxWidth
    rmax <- askMaxRibbon
    w <- measureCurLine
    n <- askNesting
    when (n + w > wmax) empty
    when (w     > rmax) empty

-- this interacts with chunk, and can either be distributive (Hughes PP) or
-- left-zero (Wadler PP) by instantiating m with [] or Maybe, (or ID for no
-- grouping) (probability monad????)
grouped :: (MonadPretty w ann fmt m) => m a -> m a
grouped aM = ifFlat aM $ (makeFlat . allowFail) aM <|> aM

text :: (MonadPretty w ann fmt m) => Text -> m ()
text t = chunk $ CText t

char :: (MonadPretty w ann fmt m) => Char -> m ()
char c = chunk $ CText $ T.pack [c]

space :: (MonadPretty w ann fmt m) => w -> m ()
space w = chunk $ CSpace w

hardLine :: (MonadPretty w ann fmt m) => m ()
hardLine = do
  tell $ PAtom ANewline
  putCurLine []

newline :: (MonadPretty w ann fmt m) => m ()
newline = do
  n <- askNesting
  hardLine
  space n

nest :: (MonadPretty w ann fmt m) => w -> m a -> m a
nest = localNesting . (+)

ifFlat :: (MonadPretty w ann fmt m) => m a -> m a -> m a
ifFlat flatAction breakAction = do
  l <- askLayout
  case l of
    Flat -> flatAction
    Break -> breakAction

makeFlat :: (MonadPretty w ann fmt m) => m a -> m a
makeFlat = localLayout $ const Flat

allowFail :: (MonadPretty w ann fmt m) => m a -> m a
allowFail = localFailure $ const CanFail

align :: (MonadPretty w ann fmt m) => m a -> m a
align aM = do
  n <- askNesting
  w :: w <- measureCurLine
  nest (w - n) aM

annotate :: (MonadPretty w ann fmt m) => ann -> m a -> m a
annotate ann aM = do
  newFormat <- askFormatAnn <*> pure ann
  pushFormat newFormat . censor (PAnn ann) $ aM

-- higher level stuff

hsep :: (MonadPretty w ann fmt m) => [m ()] -> m ()
hsep = sequence_ . intersperse (text " ")

vsep :: (MonadPretty w ann fmt m) => [m ()] -> m ()
vsep = sequence_ . intersperse newline

hvsep :: (MonadPretty w ann fmt m) => [m ()] -> m ()
hvsep = grouped . sequence_ . intersperse (ifFlat (space 1) newline)

hsepTight :: (MonadPretty w ann fmt m) => [m ()] -> m ()
hsepTight = sequence_ . intersperse (ifFlat (return ()) (space 1))

hvsepTight :: (MonadPretty w ann fmt m) => [m ()] -> m ()
hvsepTight = grouped . sequence_ . intersperse (ifFlat (return ()) newline)

collection :: (MonadPretty w ann fmt m) => m () -> m () -> m () -> [m ()] -> m ()
collection open close _   []     = open >> close
collection open close sep (x:xs) = grouped $ hvsepTight $ concat
  [ pure $ hsepTight [open, align x]
  , flip map xs $ \ x' -> hsep [sep, align x']
  , pure close
  ]

