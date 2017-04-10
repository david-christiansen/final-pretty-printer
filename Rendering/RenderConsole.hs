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

module Rendering.RenderConsole where

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

import Pretty

renderChunk :: Chunk Int -> Text
renderChunk (CText t) = t
renderChunk (CSpace w) = T.replicate w " "

renderAtom :: Atom Int -> Text
renderAtom (AChunk c) = renderChunk c
renderAtom ANewline = "\n"

render :: forall m ann . Monad m => (ann -> m ()-> m ()) -> (Text -> m ()) -> POut Int ann -> m ()
render renderAnnotation str out = render' out
  where render' :: POut Int ann -> m ()
        render' pout = case pout of
          PNull      -> str ""
          PAtom a    -> str $ renderAtom a
          PSeq o₁ o₂ -> do render' o₁
                           render' o₂
          PAnn a o   -> renderAnnotation a $ render' o

dumpDoc :: forall ann . (ann -> [SGR]) -> (ann -> StateT [ann] IO () -> StateT [ann] IO ()) -> POut Int ann -> IO ()
dumpDoc toSGR renderAnnotation = flip evalStateT [] . render renderAnnotation (lift . putStr . T.unpack) 
  
          
