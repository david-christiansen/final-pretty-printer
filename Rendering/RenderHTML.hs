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

module Rendering.RenderHTML where

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

renderChunk :: Chunk Int -> Text
renderChunk (CText t) = t
renderChunk (CSpace w) = T.replicate w " "

renderAtom :: Atom Int -> Text
renderAtom (AChunk c) = T.concatMap swapSpace $ renderChunk c
  where
    swapSpace ' ' = "&nbsp;"
    swapSpace c = T.singleton c
renderAtom ANewline = "<br/>"

render :: forall ann . (ann -> Text -> Text) -> POut Int ann -> Text
render renderAnnotation out = render' out
  where render' :: POut Int ann -> Text
        render' pout = case pout of
          PNull -> T.pack ""
          PAtom a -> renderAtom a
          PSeq o₁ o₂ -> render' o₁ `T.append` render' o₂
          PAnn a o -> renderAnnotation a $ render' o
