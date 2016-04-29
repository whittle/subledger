{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.API.Subledger.Book
       ( BookId(..)
       , BookBody(..)
       , Book(..)
       ) where

import           Control.Applicative ((<|>), empty)
import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics (Generic)

import           Data.API.Subledger.Org (OrgId(..))
import           Data.API.Subledger.Types (ResourceState(..))

newtype BookId = BookId { unBookId :: T.Text }
               deriving (Eq, Show)

data BookBody = BookBody { bookDescription :: T.Text
                         , bookReference :: Maybe T.Text
                         } deriving (Eq, Generic, Show)

instance ToJSON BookBody -- provided by Generic

data Book = Book { bookId :: BookId
                 , bookState :: ResourceState
                 , bookVersion :: Int
                 , bookBody :: BookBody
                 , bookOrgId :: OrgId
                 } deriving (Eq, Show)

mkBook :: ResourceState -> T.Text -> Int -> T.Text -> Maybe T.Text -> T.Text -> Book
mkBook s i v d mr o = Book { bookId = BookId i
                         , bookState = s
                         , bookVersion = v
                         , bookBody = BookBody { bookDescription = d
                                               , bookReference = mr
                                               }
                         , bookOrgId = OrgId o
                         }

instance FromJSON Book where
  parseJSON (Object v) = ((Active,) <$> (v .: "active_book"))
                         <|> ((Archived,) <$> (v .: "archived_book"))
                         >>= inner
    where inner (s, Object v') = mkBook s
                                 <$> v' .: "id"
                                 <*> v' .: "version"
                                 <*> v' .: "description"
                                 <*> v' .:? "reference"
                                 <*> v' .: "org"
          inner (_, _) = empty
  parseJSON _ = empty
