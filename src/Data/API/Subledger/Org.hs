{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Data.API.Subledger.Org
       ( Org(..)
       , OrgId(..)
       , OrgBody(..)
       ) where

import           Control.Applicative ((<|>))
import           Control.Monad (mzero)
import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Network.HTTP.Types.Method (methodPatch, methodPost)

import           Data.API.Subledger.Types

newtype OrgId = OrgId { unOrgId :: T.Text }
              deriving (Eq, Show)

data OrgBody = OrgBody { orgDescription :: Maybe T.Text
                       , orgReference :: Maybe T.Text
                       } deriving (Eq, Generic, Show)

instance ToJSON OrgBody where
  toJSON body = object [ "description" .= orgDescription body
                       , "reference" .= orgReference body
                       ]

data Org = Org { orgId :: OrgId
               , orgState :: ResourceState
               , orgVersion :: Int
               , orgBody :: OrgBody
               } deriving (Eq, Show)

mkOrg :: ResourceState -> T.Text -> Int -> Maybe T.Text -> Maybe T.Text -> Org
mkOrg s i v md mr = Org { orgId = OrgId i
                        , orgState = s
                        , orgVersion = v
                        , orgBody = OrgBody { orgDescription = md
                                            , orgReference = mr
                                            }
                        }

instance FromJSON Org where
  parseJSON (Object v) = ((Active,) <$> (v .: "active_org"))
                         <|> ((Archived,) <$> (v .: "archived_org"))
                         >>= inner
    where inner (s, Object v') = mkOrg s
                                 <$> v' .: "id"
                                 <*> v' .: "version"
                                 <*> v' .:? "description"
                                 <*> v' .:? "reference"
          inner (_, _) = mzero
  parseJSON _ = mzero

data CreateOrg = CreateOrg OrgBody deriving (Eq, Show)
instance Action CreateOrg OrgBody Org where
  toMethod = const methodPost
  toPathPieces = const ["orgs"]
  toBodyObject (CreateOrg body) = Just body

data FetchOrg = FetchOrg OrgId deriving (Eq, Show)
instance Action FetchOrg Void Org where
  toPathPieces (FetchOrg oid) = ["orgs", unOrgId oid]

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
