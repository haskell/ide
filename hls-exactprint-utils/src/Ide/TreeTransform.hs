{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.TreeTransform
    ( Graft(..),
      graft,
      graftMany,
      hoistGraft,
      graftWithM,
      graftWithSmallestM,
      transform,
      transformM,
      useAnnotatedSource,
      annotateParsedSource,
      ASTElement (..),
      ExceptStringT (..),
    )
where

import BasicTypes (appPrec)
import Control.Applicative (Alternative)
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Zip
import Data.Functor.Classes
import Data.Functor.Contravariant
import qualified Data.Text as T
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Rules
import Development.IDE.Core.Shake
import Development.IDE.GHC.Compat hiding (parseExpr)
import Development.IDE.Types.Location
import Generics.SYB
import Ide.PluginUtils
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Capabilities (ClientCapabilities)
import Outputable
import Retrie.ExactPrint hiding (parseDecl, parseExpr, parsePattern, parseType)
import qualified Data.DList as DL
import Data.Monoid (Ap(..))

------------------------------------------------------------------------------

-- | Get the latest version of the annotated parse source.
useAnnotatedSource ::
    String ->
    IdeState ->
    NormalizedFilePath ->
    IO (Maybe (Annotated ParsedSource))
useAnnotatedSource herald state nfp =
    fmap annotateParsedSource
        <$> runAction herald state (use GetParsedModule nfp)

annotateParsedSource :: ParsedModule -> Annotated ParsedSource
annotateParsedSource = fixAnns

------------------------------------------------------------------------------

{- | A transformation for grafting source trees together. Use the semigroup
 instance to combine 'Graft's, and run them via 'transform'.
-}
newtype Graft m a = Graft
    { runGraft :: DynFlags -> a -> TransformT m a
    }

hoistGraft :: (forall x. m x -> n x) -> Graft m a -> Graft n a
hoistGraft h (Graft f) = Graft (fmap (hoistTransform h) . f)

newtype ExceptStringT m a = ExceptStringT {runExceptString :: ExceptT String m a}
    deriving newtype
        ( MonadTrans
        , Monad
        , Functor
        , Applicative
        , Alternative
        , Foldable
        , Contravariant
        , MonadIO
        , Eq1
        , Ord1
        , Show1
        , Read1
        , MonadZip
        , MonadPlus
        , Eq
        , Ord
        , Show
        , Read
        )

instance Monad m => Fail.MonadFail (ExceptStringT m) where
    fail = ExceptStringT . ExceptT . pure . Left

instance Monad m => Semigroup (Graft m a) where
    Graft a <> Graft b = Graft $ \dflags -> a dflags >=> b dflags

instance Monad m => Monoid (Graft m a) where
    mempty = Graft $ const pure

------------------------------------------------------------------------------

-- | Convert a 'Graft' into a 'WorkspaceEdit'.
transform ::
    DynFlags ->
    ClientCapabilities ->
    Uri ->
    Graft (Either String) ParsedSource ->
    Annotated ParsedSource ->
    Either String WorkspaceEdit
transform dflags ccs uri f a = do
    let src = printA a
    a' <- transformA a $ runGraft f dflags
    let res = printA a'
    pure $ diffText ccs (uri, T.pack src) (T.pack res) IncludeDeletions

------------------------------------------------------------------------------

-- | Convert a 'Graft' into a 'WorkspaceEdit'.
transformM ::
    Monad m =>
    DynFlags ->
    ClientCapabilities ->
    Uri ->
    Graft (ExceptStringT m) ParsedSource ->
    Annotated ParsedSource ->
    m (Either String WorkspaceEdit)
transformM dflags ccs uri f a = runExceptT $
    runExceptString $ do
        let src = printA a
        a' <- transformA a $ runGraft f dflags
        let res = printA a'
        pure $ diffText ccs (uri, T.pack src) (T.pack res) IncludeDeletions

------------------------------------------------------------------------------

{- | Construct a 'Graft', replacing the node at the given 'SrcSpan' with the
 given 'LHSExpr'. The node at that position must already be a 'LHsExpr', or
 this is a no-op.
-}
graft ::
    forall ast a.
    (Data a, ASTElement ast) =>
    SrcSpan ->
    Located ast ->
    Graft (Either String) a
graft dst val = Graft $ \dflags a -> do
    (anns, val') <- annotate dflags $ maybeParensAST val
    modifyAnnsT $ mappend anns
    pure $
        everywhere'
            ( mkT $
                \case
                    (L src _ :: Located ast) | src == dst -> val'
                    l -> l
            )
            a

------------------------------------------------------------------------------

graftWithM ::
    forall ast m a.
    (Fail.MonadFail m, Data a, ASTElement ast) =>
    SrcSpan ->
    (Located ast -> TransformT m (Maybe (Located ast))) ->
    Graft m a
graftWithM dst trans = Graft $ \dflags a -> do
    everywhereM'
        ( mkM $
            \case
                val@(L src _ :: Located ast)
                    | src == dst -> do
                        mval <- trans val
                        case mval of
                            Just val' -> do
                                (anns, val'') <-
                                    hoistTransform (either Fail.fail pure) $
                                        annotate dflags $ maybeParensAST val'
                                modifyAnnsT $ mappend anns
                                pure val''
                            Nothing -> pure val
                l -> pure l
        )
        a

graftWithSmallestM ::
    forall ast m a.
    (Fail.MonadFail m, Data a, ASTElement ast) =>
    SrcSpan ->
    (Located ast -> TransformT m (Maybe (Located ast))) ->
    Graft m a
graftWithSmallestM dst trans = Graft $ \dflags a -> do
    everywhereM'
        ( mkM $
            \case
                val@(L src _ :: Located ast)
                    | dst `isSubspanOf` src -> do
                        mval <- trans val
                        case mval of
                            Just val' -> do
                                (anns, val'') <-
                                    hoistTransform (either Fail.fail pure) $
                                        annotate dflags $ maybeParensAST val'
                                modifyAnnsT $ mappend anns
                                pure val''
                            Nothing -> pure val
                l -> pure l
        )
        a

graftMany ::
    forall ast a.
    (Data a, ASTElement ast) =>
    SrcSpan ->
    [Located ast] ->
    Graft (Either String) a
graftMany dst vals = Graft $ \dflags a -> do
    everywhereM
        ( mkM $
            \case
                (ast@(L src _ :: Located ast) : rest)
                    | dst == src -> do
                        (anns, vals') <-
                            getAp $
                                foldMap
                                    ( Ap
                                        . fmap
                                            ( \(ann0, ast') ->
                                                ( setPrecedingLines ast' 1 0 ann0
                                                , DL.singleton ast'
                                                )
                                            )
                                        . annotate dflags
                                        . maybeParensAST
                                    )
                                    vals
                        modifyAnnsT $ mappend anns
                        pure $ DL.toList vals' ++ rest
                l -> pure l
        )
        a

everywhereM' :: forall m. Monad m => GenericM m -> GenericM m
everywhereM' f = go
    where
        go :: GenericM m
        go = gmapM go <=< f

class (Data ast, Outputable ast) => ASTElement ast where
    parseAST :: Parser (Located ast)
    maybeParensAST :: Located ast -> Located ast

instance p ~ GhcPs => ASTElement (HsExpr p) where
    parseAST = parseExpr
    maybeParensAST = parenthesize

instance p ~ GhcPs => ASTElement (Pat p) where
    parseAST = parsePattern
    maybeParensAST = parenthesizePat appPrec

instance p ~ GhcPs => ASTElement (HsType p) where
    parseAST = parseType
    maybeParensAST = parenthesizeHsType appPrec

instance p ~ GhcPs => ASTElement (HsDecl p) where
    parseAST = parseDecl
    maybeParensAST = id

------------------------------------------------------------------------------

-- | Dark magic I stole from retrie. No idea what it does.
fixAnns :: ParsedModule -> Annotated ParsedSource
fixAnns ParsedModule {..} =
    let ranns = relativiseApiAnns pm_parsed_source pm_annotations
     in unsafeMkA pm_parsed_source ranns 0

------------------------------------------------------------------------------

-- | Given an 'LHSExpr', compute its exactprint annotations.
annotate :: ASTElement ast => DynFlags -> Located ast -> TransformT (Either String) (Anns, Located ast)
annotate dflags ast = do
    uniq <- show <$> uniqueSrcSpanT
    let rendered = render dflags ast
    (anns, expr') <- lift $ either (Left . show) Right $ parseAST dflags uniq rendered
    let anns' = setPrecedingLines expr' 0 1 anns
    pure (anns', expr')

------------------------------------------------------------------------------

-- | Print out something 'Outputable'.
render :: Outputable a => DynFlags -> a -> String
render dflags = showSDoc dflags . ppr

------------------------------------------------------------------------------

-- | Put parentheses around an expression if required.
parenthesize :: LHsExpr GhcPs -> LHsExpr GhcPs
parenthesize = parenthesizeHsExpr appPrec
