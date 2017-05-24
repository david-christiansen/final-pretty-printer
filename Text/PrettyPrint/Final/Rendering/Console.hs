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

{-|
Module      : Text.PrettyPrint.Final.Rendering.Console
Description : A renderer for colored monospace text on a console
Copyright   : (c) David Darais, David Christiansen, and Weixi Ma 2016-2017
License     : MIT
Maintainer  : david.darais@gmail.com
Stability   : experimental
Portability : Portable

A renderer for colored monospace text on a console.
-}

module Text.PrettyPrint.Final.Rendering.Console (render, dumpDoc) where

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

import System.Console.ANSI

import Text.PrettyPrint.Final

renderChunk :: Chunk Int -> Text
renderChunk (CText t) = t
renderChunk (CSpace w) = T.replicate w " "

renderAtom :: Atom Int -> Text
renderAtom (AChunk c) = renderChunk c
renderAtom ANewline = "\n"

-- | Render a 'POut' in some monad.
render :: forall m ann . Monad m
       => (ann -> m () -> m ()) -- ^ How to transform a rendering based on an annotation
       -> (Text -> m ())        -- ^ How to output an atomic string
       -> POut Int ann          -- ^ The document to render
       -> m ()
render renderAnnotation str out = render' out
  where render' :: POut Int ann -> m ()
        render' pout = case pout of
          PNull      -> str ""
          PAtom a    -> str $ renderAtom a
          PSeq o1 o2 -> do render' o1
                           render' o2
          PAnn a o   -> renderAnnotation a $ render' o

-- | Dump pretty printer output to a console.
--
-- In 'IO' to support rendering colors on Windows.
dumpDoc :: (ann -> [SGR])
        -> (ann -> StateT [ann] IO () -> StateT [ann] IO ())
        -> POut Int ann
        -> IO ()
dumpDoc toSGR renderAnnotation =
  flip evalStateT [] .
  render renderAnnotation (lift . putStr . T.unpack)
