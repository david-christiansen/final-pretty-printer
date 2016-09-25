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

module STLCDemo where

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

import Pretty hiding (collection)
import ExtPrec hiding 
  ( parens
  , atLevel
  , bump
  , inf
  , infl
  , infr
  , pre
  , post
  , app
  )
import RenderHTML

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

-- combinators which use them

parens :: (MonadPrettyPrec w Ann fmt m) => m () -> m ()
parens = closed (annotate pun (text "(")) (annotate pun (text ")")) . align

atLevel :: (MonadPrettyPrec w Ann fmt m) => Int -> m () -> m ()
atLevel i' aM = do
  i <- askLevel
  b <- askBumped
  let aM' = localLevel (const i') $ localBumped (const False) aM
  if i < i' || (i == i' && not b)
    then aM'
    else parens aM'

bump :: (MonadPrettyPrec w Ann fmt m) => m a -> m a
bump = localBumped $ const True

inf :: (MonadPrettyPrec w Ann fmt m) => Int -> m () -> m () -> m () -> m ()
inf i oM x1M x2M = atLevel i $ hsep [ bump x1M, oM, bump x2M ]

infl :: (MonadPrettyPrec w Ann fmt m) => Int -> m () -> m () -> m () -> m ()
infl i oM x1M x2M = atLevel i $ hsep [ x1M, oM, bump x2M ]

infr :: (MonadPrettyPrec w Ann fmt m) => Int -> m () -> m () -> m () -> m ()
infr i oM x1M x2M = atLevel i $ hsep [ bump x1M, oM,  x2M ]

pre :: (MonadPrettyPrec w Ann fmt m) => Int -> m () -> m () -> m ()
pre i oM xM = atLevel i $ hsep [ oM, xM ]

post :: (MonadPrettyPrec w Ann fmt m) => Int -> m () -> m () -> m ()
post i oM xM = atLevel i $ hsep [ xM, oM ]

app :: (MonadPrettyPrec w Ann fmt m) => m () -> [m ()] -> m ()
app x xs = atLevel 100 $ hvsep $ x : map (align . bump) xs

-- baking a DocM that will work

newtype DocM a = DocM { unDocM :: RWST (PEnv Int Ann ()) (POut Int Ann) (PState Int ()) (ReaderT PrecEnv Maybe) a }
  deriving
    ( Functor, Applicative, Monad
    , MonadReader (PEnv Int Ann ()), MonadWriter (POut Int Ann), MonadState (PState Int ()), Alternative
    )

instance Measure Int () DocM where
  measure = return . runIdentity . measure

instance MonadPretty Int Ann () DocM

instance MonadReaderPrec DocM where
  askPrecEnv = DocM $ lift ask
  localPrecEnv f = DocM . mapRWST (local f) . unDocM

instance MonadPrettyPrec Int Ann () DocM

runDocM :: PEnv Int Ann () -> PrecEnv -> PState Int () -> DocM a -> Maybe (PState Int (), POut Int Ann, a)
runDocM e pe s d = (\(a,s',o) -> (s',o,a)) <$> runReaderT (runRWST (unDocM d) e s) pe

-- Doc

type Doc = DocM ()

execDoc :: Doc -> POut Int Ann
execDoc d =
  let rM = runDocM env0 precEnv0 state0 d
  in case rM of
    Nothing -> PAtom $ AChunk $ CText "<internal pretty printing error>"
    Just (_, o, ()) -> o

instance IsString Doc where
  fromString = text . fromString

instance Monoid Doc where
  mempty = return ()
  mappend = (>>)

instance Show Doc where
  show = T.unpack . render . execDoc

-- Pretty Class for this Doc

class Pretty a where
  pretty :: a -> Doc

instance Pretty Doc where
  pretty = id

instance Pretty Text where
  pretty = text . T.pack . show

instance (Pretty a) => Pretty [a] where
  pretty = collection (annotate pun "[") (annotate pun "]") (annotate pun ",") . map pretty

-- printing expressions

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
ppExp (Var x) = annotate var $ text x
ppExp (Lam x ty e) = grouped $ atLevel 10 $ nest 2 $ hvsep
  [ hsep [ annotate kwd "lam" , annotate bdr $ text x , annotate pun "." ]
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
e2 = Ifz ((Lit 1 /-/ Lit 2 /+/ (Lit 3 /-/ Lit 4)) /*/ (Lit 5 /// Lit 7))
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
