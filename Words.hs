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

module Words where

import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import Pretty

equals :: (MonadPretty w ann fmt m) => m ()
equals = char '='

parens :: (MonadPretty w ann fmt m) => m () -> m ()
parens x = char '(' >> x >> char ')'

braces :: (MonadPretty w ann fmt m) => m () -> m ()
braces x = char '{' >> x >> char '}'

colon :: (MonadPretty w ann fmt m) => m ()
colon = char ':'

comma :: (MonadPretty w ann fmt m) => m ()
comma = char ','

punctuate :: (MonadPretty w ann fmt m) => m () -> [m ()] -> [m ()] 
punctuate x es = case es of
  [] -> []
  [e] -> [e]
  (e:es) -> e : x : punctuate x es

