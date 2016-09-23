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

class Measure w m | m -> w where
  measure :: Line w -> m w

data Identity a = Identity { runIdentity :: a }
  deriving (Functor)
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity $ f x

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

instance Measure Int Identity where
  measure = pure . sum . fmap chunkLength
    where
      chunkLength (CText t) = T.length t
      chunkLength (CSpace w) = w

class
  ( Ord w, Num w
  , Measure w m
  , Monad m
  , MonadReader (PEnv w) m
  , MonadWriter (POut w ann) m
  , MonadState (PState w) m
  , Alternative m
  ) => MonadPretty (w :: *) (ann :: *) (m :: * -> *) | m -> w, m -> ann where

data Layout = Flat | Break
  deriving (Eq, Ord)
data Failure = CanFail | CantFail
  deriving (Eq, Ord)

data PEnv w = PEnv
  { maxWidth :: w
  , maxRibbon :: w
  , nesting :: w
  , layout :: Layout
  , failure :: Failure
  }
  deriving (Eq, Ord)

askMaxWidth :: (MonadReader (PEnv w) m) => m w
askMaxWidth = maxWidth <$> ask

askMaxRibbon :: (MonadReader (PEnv w) m) => m w
askMaxRibbon = maxRibbon <$> ask

askNesting :: (MonadReader (PEnv w) m) => m w
askNesting = nesting <$> ask

localNesting :: (MonadReader (PEnv w) m) => (w -> w) -> m a -> m a
localNesting f = local $ \ r -> r { nesting = f (nesting r) }

askLayout :: (MonadReader (PEnv w) m) => m Layout
askLayout = layout <$> ask

localLayout :: (MonadReader (PEnv w) m) => (Layout -> Layout) -> m a -> m a
localLayout f = local $ \ r -> r { layout = f (layout r) }

askFailure :: (MonadReader (PEnv w) m) => m Failure
askFailure = failure <$> ask

localFailure :: (MonadReader (PEnv w) m) => (Failure -> Failure) -> m a -> m a
localFailure f = local $ \ r -> r { failure = f (failure r) }

data Chunk w = CText Text | CSpace w
  deriving (Eq, Ord)

data Atom w = AChunk (Chunk w) | ANewline
  deriving (Eq, Ord)

data POut w ann = 
    PNull 
  | PAtom (Atom w)
  | PSeq (POut w ann) (POut w ann)
  | PAnn ann (POut w ann)
  deriving (Eq, Ord, Functor)
instance Monoid (POut w ann) where
  mempty = PNull
  mappend = PSeq

type Line w = [Chunk w]

data PState w = PState
  { curLine :: Line w
  }
  deriving (Eq, Ord)

getCurLine :: (MonadState (PState w) m) => m (Line w)
getCurLine = curLine <$> get

putCurLine :: (MonadState (PState w) m) => Line w -> m ()
putCurLine t = modify $ \ s -> s { curLine = t }

measureCurLine :: (Measure w m, Monad m, MonadState (PState w) m) => m w
measureCurLine = measure =<< getCurLine 

modifyLine :: (MonadState (PState w) m) => (Line w -> Line w) -> m ()
modifyLine f = modify $ \ s -> s { curLine = f (curLine s) }

-- not exported
chunk :: (MonadPretty w ann m) => Chunk w -> m ()
chunk c = do
  tell $ PAtom $ AChunk c
  modifyLine $ flip mappend [c]
  f <- askFailure
  when (f == CanFail) $ do
    wmax <- askMaxWidth
    rmax <- askMaxRibbon
    w <- measureCurLine
    n <- askNesting
    when (n + w > wmax) empty
    when (w     > rmax) empty

text :: (MonadPretty w ann m) => Text -> m ()
text t = chunk $ CText t

-- not exported
phantom :: (MonadPretty w ann m) => w -> m ()
phantom w = tell $ PAtom $ AChunk $ CSpace w

space :: (MonadPretty w ann m) => w -> m ()
space w = chunk $ CSpace w

hardLine :: (MonadPretty w ann m) => m ()
hardLine = do
  tell $ PAtom ANewline
  putCurLine []

newline :: (MonadPretty w ann m) => m ()
newline = do
  n <- askNesting
  hardLine
  space n

nest :: (MonadPretty w ann m) => w -> m a -> m a
nest = localNesting . (+)

ifFlat :: (MonadPretty w ann m) => m a -> m a -> m a
ifFlat flatAction breakAction = do
  l <- askLayout
  case l of
    Flat -> flatAction
    Break -> breakAction

makeFlat :: (MonadPretty w ann m) => m a -> m a
makeFlat = localLayout $ const Flat

allowFail :: (MonadPretty w ann m) => m a -> m a
allowFail = localFailure $ const CanFail

grouped :: (MonadPretty w ann m) => m a -> m a
grouped aM = ifFlat aM $ (makeFlat . allowFail) aM <|> aM

align :: (MonadPretty w ann m) => m a -> m a
align aM = do
  n <- askNesting
  w :: w <- measureCurLine
  nest (w - n) aM

annotate :: (MonadPretty w ann m) => ann -> m a -> m a
annotate ann aM = do
  (a, o) <- listen aM
  tell $ PAnn ann o
  return a

-- higher level stuff

hsep :: (MonadPretty w ann m) => [m ()] -> m ()
hsep = sequence_ . intersperse (text " ")

vsep :: (MonadPretty w ann m) => [m ()] -> m ()
vsep = sequence_ . intersperse newline

hvsep :: (MonadPretty w ann m) => [m ()] -> m ()
hvsep = grouped . sequence_ . intersperse (ifFlat (space 1) newline)

hsepTight :: (MonadPretty w ann m) => [m ()] -> m ()
hsepTight = sequence_ . intersperse (ifFlat (return ()) (space 1))

hvsepTight :: (MonadPretty w ann m) => [m ()] -> m ()
hvsepTight = grouped . sequence_ . intersperse (ifFlat (return ()) newline)

collection :: (MonadPretty w ann m) => Text -> Text -> Text -> [m ()] -> m ()
collection open close _   []     = text open >> text close
collection open close sep (x:xs) = grouped $ hvsepTight $ concat
  [ pure $ hsepTight [text open, align x]
  , flip map xs $ \ x' -> hsep [text sep, align x']
  , pure $ text close
  ]

--- this will go in a new file

env0 :: PEnv Int
env0 = PEnv
  { maxWidth = 80
  , maxRibbon = 60
  , layout = Break
  , failure = CantFail
  , nesting = 0
  }

state0 :: PState Int
state0 = PState
  { curLine = [] 
  }

newtype DocM a = DocM { unDocM :: RWST (PEnv Int) (POut Int ()) (PState Int) Maybe a }
  deriving 
    ( Functor, Applicative, Monad
    , MonadReader (PEnv Int), MonadWriter (POut Int ()), MonadState (PState Int), Alternative
    )
instance MonadPretty Int () DocM
runDocM :: PEnv Int -> PState Int -> DocM a -> Maybe (PState Int, POut Int (), a)
runDocM e s d = (\(a,s',o) -> (s',o,a)) <$> runRWST (unDocM d) e s

execDoc :: Doc -> POut Int ()
execDoc d =
  let rM = runDocM env0 state0 d
  in case rM of
    Nothing -> PAtom $ AChunk $ CText "<internal pretty printing error>"
    Just (_, o, ()) -> o

type Doc = DocM ()

instance Monoid Doc where
  mempty = return ()
  mappend = (>>)

class Pretty a where
  pretty :: a -> Doc
instance Pretty Doc where
  pretty = id

instance Measure Int DocM where
  measure = return . runIdentity . measure

instance Pretty Text where
  pretty = text . T.pack . show

instance (Pretty a) => Pretty [a] where
  pretty = collection "[" "]" "," . map pretty

renderChunk :: Chunk Int -> String
renderChunk (CText t) = T.unpack t
renderChunk (CSpace w) = replicate w ' '

renderAtom :: Atom Int -> String
renderAtom (AChunk c) = renderChunk c
renderAtom ANewline = "\n"

render :: POut Int () -> String
render PNull = ""
render (PAtom a) = renderAtom a
render (PSeq o1 o2) = render o1 ++ render o2
render (PAnn () o) = render o

instance Show Doc where
  show = render . execDoc
