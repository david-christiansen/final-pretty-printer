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
Module      : Text.PrettyPrint.Final.Words
Description : Convenient helpers for pretty printing
Copyright   : (c) David Darais, David Christiansen, and Weixi Ma 2016-2017
License     : MIT
Maintainer  : david.darais@gmail.com
Stability   : experimental
Portability : Portable

This module contains atomic pretty printer documents, for your convenience.
-}
module Text.PrettyPrint.Final.Words
  ( -- * Atomic documents
    equals
  , colon
  , comma
    -- * Wrappers
  , parens
  , braces
  ) where

import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import Text.PrettyPrint.Final

-- | The equals sign
equals :: (MonadPretty w ann fmt m) => m ()
equals = char '='

-- | Wrap a document in parentheses
parens :: (MonadPretty w ann fmt m) => m () -> m ()
parens x = char '(' >> x >> char ')'

-- | Wrap a document in braces
braces :: (MonadPretty w ann fmt m) => m () -> m ()
braces x = char '{' >> x >> char '}'

-- | A single colon
colon :: (MonadPretty w ann fmt m) => m ()
colon = char ':'

-- | A single comma
comma :: (MonadPretty w ann fmt m) => m ()
comma = char ','


