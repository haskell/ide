{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module Development.IDE.Spans.Common (
  showGhc
, showNameWithoutUniques
, showSD
, unqualIEWrapName
, safeTyThingId
, safeTyThingType
, SpanDoc(..)
, SpanDocUris(..)
, emptySpanDoc
, spanDocToMarkdown
, spanDocToMarkdownForTest
, DocMap
, KindMap
) where

import           Control.DeepSeq
import           Data.List.Extra
import           Data.Maybe
import qualified Data.Text                    as T
import           GHC.Generics

import           ConLike
import           DynFlags
import           GHC
import           NameEnv
import           Outputable                   hiding ((<>))
import           Var

import           Development.IDE.GHC.Compat   (oldMkUserStyle,
                                               oldRenderWithStyle)
import           Development.IDE.GHC.Orphans  ()
import           Development.IDE.GHC.Util
import qualified Documentation.Haddock.Parser as H
import qualified Documentation.Haddock.Types  as H
import           RdrName                      (rdrNameOcc)

type DocMap = NameEnv SpanDoc
type KindMap = NameEnv TyThing

showGhc :: Outputable a => a -> T.Text
showGhc = showSD . ppr

showSD :: SDoc -> T.Text
showSD = T.pack . unsafePrintSDoc

showNameWithoutUniques :: Outputable a => a -> T.Text
showNameWithoutUniques = T.pack . prettyprint
  where
    dyn = unsafeGlobalDynFlags `gopt_set` Opt_SuppressUniques
    prettyprint x = oldRenderWithStyle dyn (ppr x) style
    style = oldMkUserStyle dyn neverQualify AllTheWay

-- | Shows IEWrappedName, without any modifier, qualifier or unique identifier.
unqualIEWrapName :: IEWrappedName RdrName -> T.Text
unqualIEWrapName = showNameWithoutUniques . rdrNameOcc . ieWrappedName

-- From haskell-ide-engine/src/Haskell/Ide/Engine/Support/HieExtras.hs
safeTyThingType :: TyThing -> Maybe Type
safeTyThingType thing
  | Just i <- safeTyThingId thing = Just (varType i)
safeTyThingType (ATyCon tycon)    = Just (tyConKind tycon)
safeTyThingType _                 = Nothing

safeTyThingId :: TyThing -> Maybe Id
safeTyThingId (AnId i)           = Just i
safeTyThingId (AConLike conLike) = conLikeWrapId_maybe conLike
safeTyThingId _                  = Nothing

-- Possible documentation for an element in the code
data SpanDoc
  = SpanDocString HsDocString SpanDocUris
  | SpanDocText   [T.Text] SpanDocUris
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

data SpanDocUris =
  SpanDocUris
  { spanDocUriDoc :: Maybe T.Text -- ^ The haddock html page
  , spanDocUriSrc :: Maybe T.Text -- ^ The hyperlinked source html page
  } deriving stock (Eq, Show, Generic)
    deriving anyclass NFData

emptySpanDoc :: SpanDoc
emptySpanDoc = SpanDocText [] (SpanDocUris Nothing Nothing)

spanDocToMarkdown :: SpanDoc -> [T.Text]
spanDocToMarkdown (SpanDocString docs uris)
  = [T.pack $ haddockToMarkdown $ H.toRegular $ H._doc $ H.parseParas Nothing $ unpackHDS docs]
    <> ["\n"] <> spanDocUrisToMarkdown uris
  -- Append the extra newlines since this is markdown --- to get a visible newline,
  -- you need to have two newlines
spanDocToMarkdown (SpanDocText txt uris) = txt <> ["\n"] <> spanDocUrisToMarkdown uris

spanDocUrisToMarkdown :: SpanDocUris -> [T.Text]
spanDocUrisToMarkdown (SpanDocUris mdoc msrc) = catMaybes
  [ linkify "Documentation" <$> mdoc
  , linkify "Source" <$> msrc
  ]
  where linkify title uri = "[" <> title <> "](" <> uri <> ")"

spanDocToMarkdownForTest :: String -> String
spanDocToMarkdownForTest
  = haddockToMarkdown . H.toRegular . H._doc . H.parseParas Nothing

-- Simple (and a bit hacky) conversion from Haddock markup to Markdown
haddockToMarkdown
  :: H.DocH String String -> String

haddockToMarkdown H.DocEmpty
  = ""
haddockToMarkdown (H.DocAppend d1 d2)
  = haddockToMarkdown d1 ++ " " ++ haddockToMarkdown d2
haddockToMarkdown (H.DocString s)
  = escapeBackticks s
haddockToMarkdown (H.DocParagraph p)
  = "\n\n" ++ haddockToMarkdown p
haddockToMarkdown (H.DocIdentifier i)
  = "`" ++ i ++ "`"
haddockToMarkdown (H.DocIdentifierUnchecked i)
  = "`" ++ i ++ "`"
#if MIN_VERSION_haddock_library(1,10,0)
haddockToMarkdown (H.DocModule (H.ModLink i Nothing))
  = "`" ++ escapeBackticks i ++ "`"
-- See https://github.com/haskell/haddock/pull/1315
-- Module references can be labeled in markdown style, e.g. [some label]("Some.Module")
-- However, we don't want to use the link markup here, since the module name would be covered
-- up by the label. Thus, we keep both the label and module name in the following style:
-- some label ( `Some.Module` )
haddockToMarkdown (H.DocModule (H.ModLink i (Just label)))
  = haddockToMarkdown label ++ " ( `" ++ escapeBackticks i ++ "` )"
#else
haddockToMarkdown (H.DocModule i)
  = "`" ++ escapeBackticks i ++ "`"
#endif
haddockToMarkdown (H.DocWarning w)
  = haddockToMarkdown w
haddockToMarkdown (H.DocEmphasis d)
  = "*" ++ haddockToMarkdown d ++ "*"
haddockToMarkdown (H.DocBold d)
  = "**" ++ haddockToMarkdown d ++ "**"
haddockToMarkdown (H.DocMonospaced d)
  = "`" ++ removeUnescapedBackticks (haddockToMarkdown d) ++ "`"
haddockToMarkdown (H.DocCodeBlock d)
  = "\n```haskell\n" ++ haddockToMarkdown d ++ "\n```\n"
haddockToMarkdown (H.DocExamples es)
  = "\n```haskell\n" ++ unlines (map exampleToMarkdown es) ++ "\n```\n"
  where
    exampleToMarkdown (H.Example expr result)
      = ">>> " ++ expr ++ "\n" ++ unlines result
haddockToMarkdown (H.DocHyperlink (H.Hyperlink url Nothing))
  = "<" ++ url ++ ">"
haddockToMarkdown (H.DocHyperlink (H.Hyperlink url (Just label)))
  = "[" ++ haddockToMarkdown label ++ "](" ++ url ++ ")"
haddockToMarkdown (H.DocPic (H.Picture url Nothing))
  = "![](" ++ url ++ ")"
haddockToMarkdown (H.DocPic (H.Picture url (Just label)))
  = "![" ++ label ++ "](" ++ url ++ ")"
haddockToMarkdown (H.DocAName aname)
  = "[" ++ escapeBackticks aname ++ "]:"
haddockToMarkdown (H.DocHeader (H.Header level title))
  = replicate level '#' ++ " " ++ haddockToMarkdown title

haddockToMarkdown (H.DocUnorderedList things)
  = '\n' : (unlines $ map (("+ " ++) . trimStart . splitForList . haddockToMarkdown) things)
haddockToMarkdown (H.DocOrderedList things)
  = '\n' : (unlines $ map (("1. " ++) . trimStart . splitForList . haddockToMarkdown) things)
haddockToMarkdown (H.DocDefList things)
  = '\n' : (unlines $ map (\(term, defn) -> "+ **" ++ haddockToMarkdown term ++ "**: " ++ haddockToMarkdown defn) things)

-- we cannot render math by default
haddockToMarkdown (H.DocMathInline _)
  = "*cannot render inline math formula*"
haddockToMarkdown (H.DocMathDisplay _)
  = "\n\n*cannot render display math formula*\n\n"

-- TODO: render tables
haddockToMarkdown (H.DocTable _t)
  = "\n\n*tables are not yet supported*\n\n"

-- things I don't really know how to handle
haddockToMarkdown (H.DocProperty _)
  = ""  -- don't really know what to do

escapeBackticks :: String -> String
escapeBackticks ""       = ""
escapeBackticks ('`':ss) = '\\':'`':escapeBackticks ss
escapeBackticks (s  :ss) = s:escapeBackticks ss

removeUnescapedBackticks :: String -> String
removeUnescapedBackticks = \case
  '\\' : '`' : ss -> '\\' : '`' : removeUnescapedBackticks ss
  '`' : ss        -> removeUnescapedBackticks ss
  ""              -> ""
  s : ss          -> s : removeUnescapedBackticks ss

splitForList :: String -> String
splitForList s
  = case lines s of
      []           -> ""
      (first:rest) -> unlines $ first : map (("  " ++) . trimStart) rest
