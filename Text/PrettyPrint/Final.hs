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

{-|
Module      : Text.PrettyPrint.Final
Description : The core of the Final Pretty Printer
Copyright   : (c) David Darais, David Christiansen, and Weixi Ma 2016-2017
License     : MIT
Maintainer  : david.darais@gmail.com
Stability   : experimental
Portability : Portable

This module is the core of the Final Pretty Printer.
-}
module Text.PrettyPrint.Final
  ( -- * Pretty monads and measurement
    MonadPretty
  , Measure(..)
    -- * Atomic documents
  , text
  , char
  , space
    -- * Semantic annotations
  , annotate
    -- * Grouping, alignment, and newlines
  , newline
  , hardLine
  , ifFlat
  , grouped
  , align
  , nest
  , expr
    -- * Measuring space
  , measureText
  , spaceWidth
  , emWidth
    -- * Separators
  , hsep
  , vsep
  , hvsep
  , hsepTight
  , hvsepTight
    -- * Helpers for common tasks
  , collection
    -- * Auxiliary datatypes
  , PState(..)
  , Line
  , PEnv(..)
  , localMaxWidth
  , Failure(..)
  , Layout(..)
  , Chunk(..)
  , Atom(..)
  , POut(..)
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

-- | Strings or horizontal space to be displayed
data Chunk w =
    CText Text -- ^ An atomic string. Should not contain formatting
               -- spaces or newlines (semantic/object-level spaces OK,
               -- but not newlines)
  | CSpace w   -- ^ An amount of horizontal space to insert.
  deriving (Eq, Ord)

-- | Atomic pieces of output from the pretty printer
data Atom w =
    AChunk (Chunk w) -- ^ Inclusion of chunks
  | ANewline         -- ^ Newlines to be displayed
  deriving (Eq, Ord)

-- | A current line under consideration for insertion of breaks
type Line w fmt = [(Chunk w, fmt)]

-- | Pretty printer output represents a single annotated string.
data POut w ann =
    PNull -- ^ The empty output
  | PAtom (Atom w) -- ^ Atomic output
  | PAnn ann (POut w ann) -- ^ An annotated region of output
  | PSeq (POut w ann) (POut w ann) -- ^ The concatenation of two outputs
  deriving (Eq, Ord, Functor)

instance Monoid (POut w ann) where
  mempty = PNull
  mappend = PSeq

-- | Monad @m@ can measure lines formatted by @fmt@, getting width @w@.
--
-- For example, monospaced pretty printing can be measured in 'Identity', using
-- an 'Int' character count. For proportional fonts, @w@ will typically be something
-- like 'Double', and @m@ will be 'IO' to support observing the behavior of a font
-- rendering library.
class Measure w fmt m | m -> w, m -> fmt where
  -- | Measure a particular line
  measure :: Line w fmt -> m w

instance Measure Int () Identity where
  measure = pure . sum . fmap (chunkLength . fst)
    where
      chunkLength (CText t) = T.length t
      chunkLength (CSpace w) = w

-- | Pretty printing can be done in any pretty monad.
--
-- Pretty monads have an additional law: failure (from 'Alternative')
-- must undo the writer and state effects. So @RWST@ applied to
-- @Maybe@ is fine, but @MaybeT@ of @RWS@ is not.
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

-- | Is the pretty printer attempting to put things on one long line?
data Layout = Flat | Break
  deriving (Eq, Ord)

-- | Is there a failure handler to allow backtracking from the current line?
data Failure = CanFail | CantFail
  deriving (Eq, Ord)

-- | The dynamic context of a pretty printing computation
data PEnv w ann fmt = PEnv
  { maxWidth :: w
    -- ^ The maximum page width to use
  , maxRibbon :: w
    -- ^ The maximum amount of non-indentation space to use on one line
  , nesting :: w
    -- ^ The current indentation level
  , layout :: Layout
    -- ^ Whether lines are presently being broken or not
  , failure :: Failure
    -- ^ Whether there is a failure handler waiting to backgrack from laying out a line
  , formatting :: fmt
    -- ^ A stack of formatting codes to be combined with the monoid op
  , formatAnn :: ann -> fmt
    -- ^ A means of formatting annotations during rendering. This
    -- provides an opportunity for annotations to affect aspects of
    -- the output, like font selection, that can have an impact on the
    -- width.  If this does not agree with the formatting chosen in
    -- the final display, then odd things might happen, so the same
    -- information should be used here if possible.
  }

askMaxWidth :: (MonadReader (PEnv w ann fmt) m) => m w
askMaxWidth = maxWidth <$> ask

-- | Locally change the maximum horizontal space
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

-- | The current state of the pretty printer consists of the line under consideration.
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

-- | A line break that ignores nesting
hardLine :: (MonadPretty w ann fmt m) => m ()
hardLine = do
  tell $ PAtom ANewline
  putCurLine []

-- | A lie break that respects nesting
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

-- | Measure a string in the current pretty printing context.
--
-- Make sure to measure the text in the same dynamic context where its
-- width is to be used, to make sure the right formatting options are
-- applied.
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

-- | Separate a collection of documents with no space if they can be
-- on the same line, or with the width of a space character in
-- when they cannot.
hsepTight :: (MonadPretty w ann fmt m) => [m ()] -> m ()
hsepTight docs = do
  i <- spaceWidth
  sequence_ $ intersperse (ifFlat (return ()) (space i)) $ docs

-- | Separate a collection of documents with no space if they can be
-- on the same line, or with newlines if they cannot.
hvsepTight :: (MonadPretty w ann fmt m) => [m ()] -> m ()
hvsepTight = grouped . sequence_ . intersperse (ifFlat (return ()) newline)

-- | Print a collection in comma-initial form.
--
-- For sub-documents @d1@, @d2@, @d3@, flat mode is:
--
-- > [d1, d2, d3]
--
-- and multi-line mode is:
--
-- > [ d1
-- > , d2
-- > , d3
-- > ]
collection :: (MonadPretty w ann fmt m) => m () -> m () -> m () -> [m ()] -> m ()
collection open close _   []     = open >> close
collection open close sep (x:xs) = grouped $ hvsepTight $ concat
  [ pure $ hsepTight [open, align x]
  , flip map xs $ \ x' -> hsep [sep, align x']
  , pure close
  ]

-- | Align and group a subdocument, similar to Wadler's @group@ combinator.
expr :: MonadPretty w ann fmt m => m a -> m a
expr = align . grouped
