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
Module      : Text.PrettyPrint.Final.Rendering.HTML
Description : Monospaced text output to be included in HTML
Copyright   : (c) David Darais, David Christiansen, and Weixi Ma 2016-2017
License     : MIT
Maintainer  : david.darais@gmail.com
Stability   : experimental
Portability : Portable

HTML has its own conventions around whitespace and newlines. This
modules contains a renderer that inserts the appropriate non-breaking
spaces and line break tags.

-}

module Text.PrettyPrint.Final.Rendering.HTML (render) where

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

import Text.PrettyPrint.Final

renderChunk :: Chunk Int -> Text
renderChunk (CText t) = t
renderChunk (CSpace w) = T.replicate w " "

renderAtom :: Atom Int -> Text
renderAtom (AChunk c) = T.concatMap swapSpace $ renderChunk c
  where
    swapSpace ' ' = "&nbsp;"
    swapSpace c = T.singleton c
renderAtom ANewline = "<br/>"

-- | Render an document to a string suitable for inclusion in HTML.
-- The HTML should use a monospaced font for the code.
render :: forall ann . (ann -> Text -> Text) -> POut Int ann -> Text
render renderAnnotation out = render' out
  where render' :: POut Int ann -> Text
        render' pout = case pout of
          PNull -> T.pack ""
          PAtom a -> renderAtom a
          PSeq o₁ o₂ -> render' o₁ `T.append` render' o₂
          PAnn a o -> renderAnnotation a $ render' o
