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

module Demos.STLCDemo where

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
import Data.Map (Map)
import qualified Data.Map as Map

import Pretty hiding (collection)
import Exts.ExtPrec
import Rendering.RenderHTML

data Ann = Class Text | Tooltip Text
  deriving (Eq, Ord, Show)

-- The Language

data Ty = Int | Arr Ty Ty

data Op = Plus | Minus | Times | Div

data Exp = 
    Lit Int
  | Bin Op Exp Exp
  | Ifz Exp Exp Exp
  | Var Text
  | Lam Text Ty Exp
  | App Exp Exp
  | Raw Doc

infixl 5 /+/
infixl 5 /-/

infixl 6 /*/
infixl 6 ///

infixl 9 /@/

(/+/) :: Exp -> Exp -> Exp
(/+/) = Bin Plus

(/-/) :: Exp -> Exp -> Exp
(/-/) = Bin Minus

(/*/) :: Exp -> Exp -> Exp
(/*/) = Bin Times

(///) :: Exp -> Exp -> Exp
(///) = Bin Div

(/@/) :: Exp -> Exp -> Exp
(/@/) = App

-- The Pretty Printer

-- class shortcuts

lit :: Ann
lit = Class "lit"

var :: Ann
var = Class "var"

pun :: Ann
pun = Class "pun"

bdr :: Ann
bdr = Class "bdr"

kwd :: Ann
kwd = Class "kwd"

opr :: Ann
opr = Class "opr"

type TEnv = Map Text Text

tEnv0 :: TEnv
tEnv0 = Map.empty

newtype DocM a = DocM { unDocM :: PrecT Ann (RWST (PEnv Int Ann ()) (POut Int Ann) (PState Int ()) (ReaderT TEnv Maybe)) a }
  deriving
    ( Functor, Applicative, Monad, Alternative
    , MonadReader (PEnv Int Ann ()), MonadWriter (POut Int Ann), MonadState (PState Int ())
    , MonadReaderPrec Ann
    )
instance MonadPretty Int Ann () DocM
instance MonadPrettyPrec Int Ann () DocM

instance Measure Int () DocM where
  measure = return . runIdentity . measure

runDocM :: PEnv Int Ann () -> PrecEnv Ann -> TEnv -> PState Int () -> DocM a -> Maybe (PState Int (), POut Int Ann, a)
runDocM e pe te s d = (\(a,s',o) -> (s',o,a)) <$> runReaderT (runRWST (runPrecT pe $ unDocM d) e s) te

askTEnv :: DocM TEnv
askTEnv = DocM $ lift $ lift ask

localTEnv :: (TEnv -> TEnv) -> DocM a -> DocM a
localTEnv f = DocM . mapPrecT (mapRWST (local f)) . unDocM

-- Doc

type Doc = DocM ()

execDoc :: Doc -> POut Int Ann
execDoc d =
  let rM = runDocM env0 precEnv0 tEnv0 state0 d
  in case rM of
    Nothing -> PAtom $ AChunk $ CText "<internal pretty printing error>"
    Just (_, o, ()) -> o

instance IsString Doc where
  fromString = text . fromString

instance Monoid Doc where
  mempty = return ()
  mappend = (>>)

renderAnnotation :: Ann -> Text -> Text
renderAnnotation (Class c) t = mconcat [ "<span class='" , c , "'>" , t , "</span>" ]
renderAnnotation (Tooltip p) t = mconcat [ "<span title='" , p , "'>" , t , "</span>" ]

instance Show Doc where
  show = T.unpack . (render renderAnnotation) . execDoc

-- Pretty Class for this Doc

class Pretty a where
  pretty :: a -> Doc

instance Pretty Doc where
  pretty = id

instance Pretty Text where
  pretty = text . T.pack . show

-- printing expressions

ftTy :: Ty -> Text
ftTy Int = "Int"
ftTy (Arr t1 t2) = ftTy t1 `T.append` " -> " `T.append` ftTy t2

ppOp :: Op -> Doc -> Doc -> Doc
ppOp Plus x1 x2 = infl 20 (annotate opr "+") (grouped x1) (grouped x2)
ppOp Minus x1 x2 = infl 20 (annotate opr "-") (grouped x1) (grouped x2)
ppOp Times x1 x2 = infl 30 (annotate opr "*") (grouped x1) (grouped x2)
ppOp Div x1 x2 = infl 30 (annotate opr "/") (grouped x1) (grouped x2)

ppExp :: Exp -> Doc
ppExp (Lit i) = annotate lit $ text $ T.pack $ show i
ppExp (Bin o e1 e2) = ppOp o (ppExp e1) (ppExp e2)
ppExp (Ifz e1 e2 e3) = grouped $ atLevel 10 $ hvsep 
  [ grouped $ nest 2 $ hvsep [ annotate kwd "ifz" , botLevel $ ppExp e1 ]
  , grouped $ nest 2 $ hvsep [ annotate kwd "then" , botLevel $ ppExp e2 ]
  , grouped $ nest 2 $ hvsep [ annotate kwd "else" , ppExp e3 ]
  ]
ppExp (Var x) = do
  tEnv <- askTEnv
  let tt = tEnv Map.! x
  annotate (Tooltip tt) $ annotate var $ text x
ppExp (Lam x ty e) = localTEnv (Map.insert x $ ftTy ty) $ grouped $ atLevel 10 $ nest 2 $ hvsep
  [ hsep [ annotate kwd "lam" , annotate (Tooltip $ ftTy ty) $ annotate bdr $ text x , annotate pun "." ]
  , ppExp e
  ]
ppExp (App e1 e2) = app (ppExp e1) [ppExp e2]
ppExp (Raw d) = d

precDebug :: Doc
precDebug = do
  lvl <- askLevel
  bmp <- askBumped
  text $ "p:" `T.append` T.pack (show lvl) `T.append` (if bmp then "B" else "")

instance Pretty Exp where
  pretty = ppExp

e1 :: Exp
e1 = Lam "x" Int $ Var "x"

-- ifz ((1 - 2) + (3 - 4)) * (5 / 7) 
-- then lam x . x 
-- else (lam y . y) (ifz 1 then 2 else 3)
e2 :: Exp
e2 = Ifz ((Lit 1 /-/ Lit 2 /+/ (Lit 3 /-/ Lit 4)) /*/ (Lit 5 /// Lit 7) /+/ Lit 8)
         (Lam "x" Int $ Var "x")
         ((Lam "y" Int $ Var "y") /@/ (Ifz (Lit 1) (Lit 2) (Lit 3)))

-- run this file to output an html file "stlc_demo.html", which links against
-- "stlc_demo.css"
main :: IO ()
main = do
  let output = show $ localMaxWidth (const 15) $ pretty e2
  putStrLn output
  writeFile "stlc_demo.html" $ concat
    [ "<html><head><link rel='stylesheet' type='text/css' href='stlc_demo.css'></head><body><p>"
    , output
    , "</p></body></html>"
    ]
