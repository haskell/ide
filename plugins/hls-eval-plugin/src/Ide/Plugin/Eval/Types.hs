{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wwarn #-}

module Ide.Plugin.Eval.Types (
    locate,
    locate0,
    Test (..),
    isProperty,
    Format (..),
    Language (..),
    Section (..),
    Sections(..),
    hasTests,
    hasPropertyTest,
    splitSections,
    Loc,
    Located (..),
    Comments(..),
    unLoc,
    Txt,
) where

import Control.DeepSeq (NFData (rnf), deepseq)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString (..))
import GHC.Generics (Generic)
import Data.Map.Strict (Map)
import Development.IDE.GHC.Compat (RealSrcSpan)

-- | A thing with a location attached.
data Located l a = Located {location :: l, located :: a}
    deriving (Eq, Show, Ord, Functor, Generic, FromJSON, ToJSON)

-- | Discard location information.
unLoc :: Located l a -> a
unLoc (Located _ a) = a

instance (NFData l, NFData a) => NFData (Located l a) where
    rnf (Located loc a) = loc `deepseq` a `deepseq` ()

type Loc = Located Line

type Line = Int

locate :: Loc [a] -> [Loc a]
locate (Located l tst) = zipWith Located [l ..] tst

locate0 :: [a] -> [Loc a]
locate0 = locate . Located 0

type Txt = String

data Sections =
    Sections
    { setupSections :: [Section]
    , lineSecions :: [Section]
    , multilneSections :: [Section]
    }
    deriving (Show, Eq, Generic)

data Section = Section
    { sectionName :: Txt
    , sectionTests :: [Loc Test]
    , sectionLanguage :: Language
    , sectionFormat :: Format
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON, NFData)

hasTests :: Section -> Bool
hasTests = not . null . sectionTests

hasPropertyTest :: Section -> Bool
hasPropertyTest = any (isProperty . unLoc) . sectionTests

-- |Split setup and normal sections
splitSections :: [Section] -> ([Section], [Section])
splitSections = partition ((== "setup") . sectionName)

data Test
    = Example {testLines :: NonEmpty Txt, testOutput :: [Txt]}
    | Property {testline :: Txt, testOutput :: [Txt]}
    deriving (Eq, Show, Generic, FromJSON, ToJSON, NFData)

data Comments =
    Comments
        { lineComments :: Map RealSrcSpan String
        , blockComments :: Map RealSrcSpan String
        }
    deriving (Show, Eq, Ord, Generic)

instance Semigroup Comments where
    Comments ls bs <> Comments ls' bs' = Comments (ls <> ls') (bs <> bs')

instance Monoid Comments where
    mempty = Comments mempty mempty

isProperty :: Test -> Bool
isProperty (Property _ _) = True
isProperty _ = False

data Format = SingleLine | MultiLine deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON, NFData)

data Language = Plain | Haddock deriving (Eq, Show, Generic, Ord, FromJSON, ToJSON, NFData)

data ExpectedLine = ExpectedLine [LineChunk] | WildCardLine
    deriving (Eq, Show, Generic, FromJSON, ToJSON, NFData)

instance IsString ExpectedLine where
    fromString = ExpectedLine . return . LineChunk

data LineChunk = LineChunk String | WildCardChunk
    deriving (Eq, Show, Generic, FromJSON, ToJSON, NFData)

instance IsString LineChunk where
    fromString = LineChunk
