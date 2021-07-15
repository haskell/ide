{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Ide.Plugin.CallHierarchy.Types where

import           Data.Aeson
import           Database.SQLite.Simple
import           Database.SQLite.Simple.ToField
import           GHC.Generics

data Vertex = Vertex {
  mod    :: String
, occ    :: String
, hieSrc :: FilePath
, sl     :: Int
, sc     :: Int
, el     :: Int
, ec     :: Int
, casl   :: Int -- sl for call appear
, casc   :: Int -- sc for call appear
, cael   :: Int -- el for call appear
, caec   :: Int -- ec for call appear
} deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance ToRow Vertex where
  toRow (Vertex a b c d e f g h i j k) =
    [ toField a, toField b, toField c, toField d
    , toField e, toField f, toField g, toField h
    , toField i, toField j, toField k
    ]

instance FromRow Vertex where
  fromRow = Vertex <$> field <*> field <*> field
                   <*> field <*> field <*> field
                   <*> field <*> field <*> field
                   <*> field <*> field
