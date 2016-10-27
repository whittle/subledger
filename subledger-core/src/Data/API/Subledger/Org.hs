{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Data.API.Subledger.Org
       ( Org(..)
       , OrgId(..)
       , OrgBody(..)
       , createOrg
       , fetchOrg
       ) where

import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Aeson.Types (Options(..))
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Network.HTTP.Types.Method (methodPatch, methodPost)

import           Data.API.Subledger.Request
import           Data.API.Subledger.Types
import           Data.API.Subledger.Util

newtype OrgId = OrgId { unOrgId :: T.Text }
              deriving (Eq, Show)

instance FromJSON OrgId where
  parseJSON (String s) = pure $ OrgId s
  parseJSON _ = mempty

data OrgBody = OrgBody { orgBodyDescription :: Maybe T.Text
                       , orgBodyReference :: Maybe T.Text
                       } deriving (Eq, Generic, Show)

orgBodyFields :: String -> String
orgBodyFields = snakeCase . drop 7

instance FromJSON OrgBody where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = orgBodyFields }

instance ToJSON OrgBody where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = orgBodyFields }

data Org = Org { orgId :: OrgId
               , orgState :: ResourceState
               , orgVersion :: Int
               , orgBody :: OrgBody
               } deriving (Eq, Show)

instance FromJSON Org where
  parseJSON (Object v) = ((Active,) <$> (v .: "active_org"))
                     <|> ((Archived,) <$> (v .: "archived_org"))
                     >>= inner
    where inner (s, Object v') = Org <$> v' .: "id"
                                     <*> pure s
                                     <*> v' .: "version"
                                     <*> parseJSON (Object v')
          inner (_, _) = mempty
  parseJSON _ = mempty

data CreateOrg
type instance SubledgerReturn CreateOrg = Org

createOrg :: T.Text -> SubledgerRequest CreateOrg
createOrg description = mkRequest POST ["orgs"] $
                        OrgBody { orgBodyDescription = Just description
                                , orgBodyReference = Nothing
                                }

data FetchOrg
type instance SubledgerReturn FetchOrg = Org

fetchOrg :: OrgId -> SubledgerRequest FetchOrg
fetchOrg oid = mkEmptyRequest GET ["orgs", unOrgId oid]

data PatchOrg = PatchOrg Org deriving (Eq, Show)
instance Action PatchOrg OrgBody Org where
  toMethod = const methodPatch
  toPathPieces (PatchOrg org) = ["orgs", unOrgId $ orgId org]
  toBodyObject (PatchOrg org) = Just $ orgBody org

data ArchiveOrg = ArchiveOrg OrgId deriving (Eq, Show)
instance Action ArchiveOrg Void Org where
  toMethod = const methodPost
  toPathPieces (ArchiveOrg oid) = ["orgs", unOrgId oid, "archive"]

data ActivateOrg = ActivateOrg OrgId deriving (Eq, Show)
instance Action ActivateOrg Void Org where
  toMethod = const methodPost
  toPathPieces (ActivateOrg oid) = ["orgs", unOrgId oid, "activate"]
