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

module Text.PrettyPrint.Final
  ( MonadPretty
  , Measure(..)
  , PState(..)
  , PEnv(..)
  , Failure(..)
  , Layout(..)
  , Chunk(..)
  , Atom(..)
  , POut(..)
  , align
  , space
  , collection
  , grouped
  , expr
  , nest
  , text
  , char
  , annotate
  , hsep
  , vsep
  , hvsep
  , hsepTight
  , hvsepTight
  , localMaxWidth
  , measureText
  , spaceWidth
  , emWidth
  , newline
  , ifFlat
  ) where

import Control.Monad
import Control.Applicative
import Control.Monad.Identity
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
  ) => MonadPretty w ann fmt m
       | m -> w, m -> ann, m -> fmt
  where

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

-- grouped interacts with chunk, and can either be distributive (Hughes PP) or
-- left-zero (Wadler PP) by instantiating m with [] or Maybe, (or ID for no
-- grouping) (probability monad????)

-- | Group a collection of pretty-printer actions, undoing their newlines if possible.
-- If m is [], grouping has a distributive Hughes-style semantics, and if m is Maybe,
-- then grouping has a Wadler-style left-zero semantics. The identity monad gives no
-- grouping.
grouped :: (MonadPretty w ann fmt m) => m a -> m a
grouped aM = ifFlat aM $ (makeFlat . allowFail) aM <|> aM

-- | Include a Text string in the document.
text :: (MonadPretty w ann fmt m) => Text -> m ()
text t = chunk $ CText t

-- | Include a single character in the document.
char :: (MonadPretty w ann fmt m) => Char -> m ()
char c = chunk $ CText $ T.pack [c]

-- | Include a space of a given width in the document.
space :: (MonadPretty w ann fmt m) => w -> m ()
space w = chunk $ CSpace w

-- | Include a line break that will not be undone by grouping. This is to support
-- languages with semantically-meaningful newlines.
hardLine :: (MonadPretty w ann fmt m) => m ()
hardLine = do
  tell $ PAtom ANewline
  putCurLine []

-- | Include a line break that will not be undone by grouping and that
-- respects the current nesting level.
newline :: (MonadPretty w ann fmt m) => m ()
newline = do
  n <- askNesting
  hardLine
  space n

-- | Increase the nesting level to render some argument, which will result in the
-- document being indented following newlines.
nest :: (MonadPretty w ann fmt m) => w -> m a -> m a
nest = localNesting . (+)

-- | Conditionally render documents based on whether grouping is undoing newlines.
ifFlat :: (MonadPretty w ann fmt m) => m a -> m a -> m a
ifFlat flatAction breakAction = do
  l <- askLayout
  case l of
    Flat -> flatAction
    Break -> breakAction

-- | Unconditionally undo newlines in a document.
makeFlat :: (MonadPretty w ann fmt m) => m a -> m a
makeFlat = localLayout $ const Flat

allowFail :: (MonadPretty w ann fmt m) => m a -> m a
allowFail = localFailure $ const CanFail

-- | Vertically align documents.
align :: (MonadPretty w ann fmt m) => m a -> m a
align aM = do
  n <- askNesting
  w :: w <- measureCurLine
  nest (w - n) aM

-- | Add a semantic annotation to a document. These annotations are converted into
-- the output stream's notion of decoration by the renderer.
annotate :: (MonadPretty w ann fmt m) => ann -> m a -> m a
annotate ann aM = do
  newFormat <- askFormatAnn <*> pure ann
  pushFormat newFormat . censor (PAnn ann) $ aM

-- higher level stuff

-- | Separate a collection of documents with a space character.
hsep :: (MonadPretty w ann fmt m) => [m ()] -> m ()
hsep = sequence_ . intersperse (text " ")

-- | Separate a collection of documents with newlines.
vsep :: (MonadPretty w ann fmt m) => [m ()] -> m ()
vsep = sequence_ . intersperse newline

measureText :: (MonadPretty w ann fmt m) => Text -> m w
measureText txt = do
  format <- askFormat
  measure [(CText txt, format)]

-- | Measure the width of a space in the current font
spaceWidth :: (MonadPretty w ann fmt m) => m w
spaceWidth = measureText " "

-- | Measure the width of a capital M in the current font
emWidth :: (MonadPretty w ann fmt m) => m w
emWidth = measureText "M"

-- | Separate a collection of documents with a space (if there's room)
-- or a newline if not.
hvsep :: (MonadPretty w ann fmt m) => [m ()] -> m ()
hvsep docs = do
  i <- spaceWidth
  grouped $ sequence_ $ intersperse (ifFlat (space i) newline) $ docs

hsepTight :: (MonadPretty w ann fmt m) => [m ()] -> m ()
hsepTight docs = do
  i <- spaceWidth
  sequence_ $ intersperse (ifFlat (return ()) (space i)) $ docs

hvsepTight :: (MonadPretty w ann fmt m) => [m ()] -> m ()
hvsepTight = grouped . sequence_ . intersperse (ifFlat (return ()) newline)

collection :: (MonadPretty w ann fmt m) => m () -> m () -> m () -> [m ()] -> m ()
collection open close _   []     = open >> close
collection open close sep (x:xs) = grouped $ hvsepTight $ concat
  [ pure $ hsepTight [open, align x]
  , flip map xs $ \ x' -> hsep [sep, align x']
  , pure close
  ]

-- | Align and group a subdocument, similar to Wadler's group combinator.
expr :: MonadPretty w ann fmt m => m a -> m a
expr = align . grouped
