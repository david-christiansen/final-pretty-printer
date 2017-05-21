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

module Text.PrettyPrint.Final.Rendering.PlainText (hPutDoc, saveDoc, tempDoc) where

import Control.Monad
import Control.Applicative
import Control.Monad.Catch(MonadMask)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Data.List
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO
import System.IO.Temp

import Text.PrettyPrint.Final

hRenderChunk :: MonadIO m => Handle -> Chunk Int -> m ()
hRenderChunk h (CText t) = liftIO $ TIO.hPutStr h t
hRenderChunk h (CSpace w) = replicateM_ w (liftIO $ TIO.hPutStr h " ")

hRenderAtom :: MonadIO m => Handle -> Atom Int -> m ()
hRenderAtom h (AChunk c) = hRenderChunk h c
hRenderAtom h ANewline = liftIO $ TIO.hPutStr h "\n"

hPutDoc :: MonadIO m => Handle -> POut Int ann -> m ()
hPutDoc h out = render' out
  where
        render' pout = case pout of
          PNull      -> return ()
          PAtom a    -> hRenderAtom h a
          PSeq o1 o2 -> render' o1 >> render' o2
          PAnn a o   -> render' o

saveDoc :: MonadIO m => FilePath -> IOMode -> POut Int ann -> m ()
saveDoc f mode out = liftIO $ withFile f mode (flip hPutDoc out)

tempDoc :: (MonadIO m, MonadMask m) => POut Int ann -> m ()
tempDoc out = withSystemTempFile "pretty.txt" (\_ h -> hPutDoc h out)
