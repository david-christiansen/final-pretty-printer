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

module ListDemo where

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

import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango.Cairo
import Graphics.Rendering.Pango.Context
import Graphics.Rendering.Pango.Layout

import Pretty

getWidth :: PangoRectangle -> Double
getWidth (PangoRectangle x y w h) = w

getLayoutWidth :: Text -> IO Double
getLayoutWidth t = do
  ctxt <- cairoCreateContext Nothing
  lay <- layoutText ctxt t
  getWidth . fst <$> layoutGetExtents lay

getLayoutWidthMarkup :: Text -> IO Double
getLayoutWidthMarkup t = do
  ctxt <- cairoCreateContext Nothing
  lay <- layoutEmpty ctxt
  (layoutSetMarkup lay t) :: IO String
  getWidth . fst <$> layoutGetExtents lay


-- | Valid Pango sizes
data Size = SizeInt Int | XXSmall | XSmall | Small | Medium | Large | XLarge | XXLarge | Smaller | Larger

-- | A small subset of Pango formatting
data PangoFmt = Size Size | Bold | Italic | FG Text

makeMarkup :: [PangoFmt] -> Text -> Text
makeMarkup []              txt =
  escapeMarkup txt
makeMarkup (Size s : fmts) txt =
  T.concat [ (T.pack $ "<span size=\"" ++ sizeStr s ++ "\">") 
           , makeMarkup fmts txt
           , "</span>"
           ]
    where
      sizeStr (SizeInt x) = show x
      sizeStr XXSmall     = "xx-small"
      sizeStr XSmall      = "x-small"
      sizeStr Small       = "small"
      sizeStr Medium      = "medium"
      sizeStr Large       = "large"
      sizeStr XLarge      = "x-large"
      sizeStr XXLarge     = "xx-large"
      sizeStr Smaller     = "smaller"
      sizeStr Larger      = "larger"
makeMarkup (Bold : fmts) txt   =
  T.concat ["<b>" , makeMarkup fmts txt, "</b>"]
makeMarkup (Italic : fmts) txt   =
  T.concat ["<i>" , makeMarkup fmts txt, "</i>"]
makeMarkup (FG fg : fmts) txt   =
  T.concat [ "<span color=\""
           , fg
           , "\">"
           , makeMarkup fmts txt
           , "</i>"]


-- | The language to print: expressions
data Expr = CstI Int | Add Expr Expr

-- | The language to print: statements
data Stmt = Skip | Print Expr | Dotimes Expr [Expr]

-- | Top-level programs
newtype Prog = Prog [Stmt]

data Ann = Kwd | Num | Op

env0 :: PEnv Double Ann [PangoFmt]
env0 = PEnv
  { maxWidth = 80.0
  , maxRibbon = 60.0
  , layout = Break
  , failure = CantFail
  , nesting = 0
  , formatting = mempty
  , formatAnn =
    \ annot ->
      case annot of
        Kwd -> [Bold]
        Op  -> [Italic]
        _   -> []
  }

state0 :: PState Double [PangoFmt]
state0 = PState
  { curLine = []
  }

-- For plain text pretty printing
newtype DocM a = DocM { unDocM :: RWST (PEnv Double Ann [PangoFmt]) (POut Double Ann) (PState Double [PangoFmt]) IO a }
  deriving
    ( Functor, Applicative, Monad
    , MonadReader (PEnv Double Ann [PangoFmt]), MonadWriter (POut Double Ann), MonadState (PState Double [PangoFmt]), Alternative
    )

runIO :: IO a -> DocM a
runIO m = DocM (lift m)

instance MonadPretty Double Ann [PangoFmt] DocM

instance IsString (DocM ()) where
  fromString = text . fromString

runDocM :: PEnv Double Ann [PangoFmt] -> PState Double [PangoFmt] -> DocM a -> IO (PState Double [PangoFmt], POut Double Ann, a)
runDocM e s d = (\(a,s',o) -> (s',o,a)) <$> runRWST (unDocM d) e s

execDoc :: Doc -> IO (POut Double Ann)
execDoc d =
   (\ (_, o, _) -> o) <$> runDocM env0 state0 d


type Doc = DocM ()

instance Monoid Doc where
  mempty = return ()
  mappend = (>>)

class Pretty a where
  pretty :: a -> Doc

instance Pretty Doc where
  pretty = id

instance Measure Double [PangoFmt] DocM where
  measure t = sum <$> mapM measureChunk t
    where
      measureChunk ((CText txt), fmt) =
        runIO . getLayoutWidthMarkup $ makeMarkup fmt txt
      measureChunk ((CSpace w),  fmt) =
        pure w


instance Pretty Expr where
  pretty (CstI i) =
    annotate Num (text (T.pack (show i)))
  pretty (Add e1 e2) =
    collection
      (text "(") (text ")")
      (annotate Op (text "+"))
      [pretty e1, pretty e2]

instance Pretty Stmt where
  pretty Skip =
    annotate Kwd (text "skip")
  pretty (Print e) =
    nest 5.0 $
      hvsep [ annotate Kwd (text "print")
            , pretty e
            ]
  pretty (Dotimes e body) =
    nest 5.0 $
      hvsep [ annotate Kwd (text "dotimes")
            , text "(" >> pretty e >> text ")"
            , collection "{" "}" ";" (map pretty body)
            ]

instance Pretty Prog where
  pretty (Prog stmts) =
    collection empty empty ";" (map pretty stmts)


-- | Rendering state
data RenderState = MkRenderState
  { renderLineHeight :: Double
  , renderLineWidth :: Double
  , renderFormatting :: [PangoFmt]
  }

data RenderContext = MkRenderContext
  { renderContext :: PangoContext }

type Renderer a = ReaderT RenderContext (StateT RenderState IO) a

askContext :: Renderer PangoContext
askContext = renderContext <$> ask

-- A little test to work out the API
testImage :: String -> IO ()
testImage outname = do
  withImageSurface FormatARGB32 800 600 $ \ surf -> do
    fopts <- fontOptionsCreate
    fontOptionsSetAntialias fopts AntialiasGray
    ctxt <- cairoCreateContext Nothing
    lay <- layoutEmpty ctxt
    (layoutSetMarkup lay (T.pack "<i>hello</i>\n world") :: IO String)
    renderWith surf $ do
      setFontOptions fopts
      setSourceRGB 1 1 1
      rectangle 0.0 0.0 800.0 600.0
      fill
      setSourceRGB 0 0 0
      layoutPath lay
      fill
    surfaceWriteToPNG surf outname

-- terpri :: Renderer ()
-- terpri = do
--   h <- renderLineHeight <$> get
--   w <- renderLineHeight <$> get
--   (x, y) <- lift $ lift getCurrentPoint
--   lift . lift $ moveTo (x - w) (h + y)
--   modify $ \ s ->
--     s { renderLineHeight = 0.0
--       , renderLineWidth  = 0.0
--       }

-- forward :: Double -> Renderer ()
-- forward w = do
--   (x, y) <- lift . lift $ getCurrentPoint
--   modify $ \ s ->
--     s { renderLineWidth = renderLineWidth s + w }
--   lift . lift $ moveTo (x + w) y

-- addStr :: Text -> Renderer ()
-- addStr txt = do
--   fmt <- renderFormatting <$> get
--   let withMarkup = makeMarkup fmt txt
--   layou <- 
--   (x, y) <- lift . lift $ getCurrentPoint
--   lift . lift $ layoutPath withMarkup

-- renderChunk :: Chunk Double -> String
-- renderChunk (CText t) = T.unpack t
-- renderChunk (CSpace w) = replicate w ' '

-- renderAtom :: Atom Double -> String
-- renderAtom (AChunk c) = renderChunk c
-- renderAtom ANewline = "\n"

-- render :: forall m ann .
--           Monad m
--        => (ann -> m ()) -> (ann -> m ())
--        -> (String -> m ())
--        -> POut Double ann -> m ()
-- render start end str out = render' out
--   where
--     render' :: POut Double ann -> m ()
--     render' PNull = str ""
--     render' (PAtom a) = str $ renderAtom a
--     render' (PSeq o1 o2) = do
--       render' o1
--       render' o2
--     render' (PAnn a o) = start a >> render' o >> end a

