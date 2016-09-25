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

module RenderHTML where

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

data Ann = Class Text | Tooltip Text
  deriving (Eq, Ord, Show)

renderChunk :: Chunk Int -> Text
renderChunk (CText t) = t
renderChunk (CSpace w) = T.replicate w " "

renderAtom :: Atom Int -> Text
renderAtom (AChunk c) = T.concatMap swapSpace $ renderChunk c
  where
    swapSpace ' ' = "&nbsp;"
    swapSpace c = T.singleton c
renderAtom ANewline = "<br/>"

renderAnnotation :: Ann -> Text -> Text
renderAnnotation (Class c) t = mconcat [ "<span class='" , c , "'>" , t , "</span>" ]
renderAnnotation (Tooltip p) t = mconcat [ "<span title='" , p , "'>" , t , "</span>" ]

render :: POut Int Ann -> Text
render PNull = ""
render (PAtom a) = renderAtom a
render (PSeq o1 o2) = render o1 `T.append` render o2
render (PAnn a o) = renderAnnotation a $ render o
