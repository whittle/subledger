{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Data.API.Subledger.Types (ResourceState(..))

newtype OrgId = OrgId { unOrgId :: T.Text }
              deriving (Eq, Show)

data OrgBody = OrgBody { orgDescription :: Maybe T.Text
                       , orgReference :: Maybe T.Text
                       } deriving (Eq, Generic, Show)

instance ToJSON OrgBody -- provided by Generic

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
