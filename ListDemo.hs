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

module ListDemo where

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

-- Constructor names or built-in syntax
data HsAnn = Ctor | Stx
  deriving (Eq, Ord, Show)

env0 :: Monoid fmt => PEnv Int a fmt
env0 = PEnv
  { maxWidth = 80
  , maxRibbon = 60
  , layout = Break
  , failure = CantFail
  , nesting = 0
  , formatting = []
  , formatAnn = const mempty
  }

state0 :: PState Int ()
state0 = PState
  { curLine = []
  }

newtype DocM a = DocM { unDocM :: RWST (PEnv Int HsAnn ()) (POut Int HsAnn) (PState Int ()) Maybe a }
  deriving
    ( Functor, Applicative, Monad
    , MonadReader (PEnv Int HsAnn ()), MonadWriter (POut Int HsAnn), MonadState (PState Int ()), Alternative
    )

instance MonadPretty Int HsAnn () DocM

instance IsString (DocM ()) where
  fromString = text . fromString

runDocM :: PEnv Int HsAnn () -> PState Int () -> DocM a -> Maybe (PState Int (), POut Int HsAnn, a)
runDocM e s d = (\(a,s',o) -> (s',o,a)) <$> runRWST (unDocM d) e s

execDoc :: Doc -> POut Int HsAnn
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

instance Measure Int () DocM where
  measure = return . runIdentity . measure

instance Pretty Text where
  pretty = text . T.pack . show

instance (Pretty a) => Pretty [a] where
  pretty = collection (annotate Stx "[") (annotate Stx "]") (annotate Stx ",") . map pretty

renderChunk :: Chunk Int -> String
renderChunk (CText t) = T.unpack t
renderChunk (CSpace w) = replicate w ' '

renderAtom :: Atom Int -> String
renderAtom (AChunk c) = renderChunk c
renderAtom ANewline = "\n"

render :: forall m ann . Monad m => (ann -> m ()) -> (ann -> m ()) -> (String -> m ()) -> POut Int ann -> m ()
render start end str out = flip evalStateT [] $ render' out
  where
    push :: ann -> StateT [ann] m ()
    push a = modify (a:)

    pop :: StateT [ann] m ann
    pop = do
      (a:as) <- get
      put as
      pure a

    render' :: POut Int ann -> StateT [ann] m ()
    render' PNull = lift $ str ""
    render' (PAtom a) = lift . str $ renderAtom a
    render' (PSeq o1 o2) = do
      render' o1
      render' o2
    render' (PAnnStart a) = push a >> lift (start a)
    render' PAnnEnd = pop >>= lift . end


instance Show Doc where
  show = snd . runWriter . render (const (pure ())) (const (pure ())) tell . execDoc

toHtml :: Doc -> String
toHtml = snd . runWriter . render openTag closeTag tell . execDoc
  where openTag Ctor = tell "<span class=\"constructor\">"
        openTag Stx  = tell "<span class=\"syntax\">"
        closeTag _   = tell "</span>"
