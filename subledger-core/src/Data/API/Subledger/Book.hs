{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Data.API.Subledger.Book
       ( BookId(..)
       , BookBody(..)
       , Book(..)
       ) where

import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Aeson.Types (Options(..))
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Network.HTTP.Types.Method (methodPatch, methodPost)

import           Data.API.Subledger.Org (OrgId(..))
import           Data.API.Subledger.Types
import           Data.API.Subledger.Util

newtype BookId = BookId { unBookId :: T.Text }
               deriving (Eq, Show)

instance FromJSON BookId where
  parseJSON (String s) = pure $ BookId s
  parseJSON _ = mempty

data BookBody = BookBody { bookBodyDescription :: T.Text
                         , bookBodyReference :: Maybe T.Text
                         } deriving (Eq, Generic, Show)

bookBodyFields :: String -> String
bookBodyFields = snakeCase . drop 8

instance FromJSON BookBody where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = bookBodyFields }

instance ToJSON BookBody where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = bookBodyFields }

data Book = Book { bookId :: BookId
                 , bookState :: ResourceState
                 , bookVersion :: Int
                 , bookBody :: BookBody
                 , bookOrgId :: OrgId
                 } deriving (Eq, Show)

instance FromJSON Book where
  parseJSON (Object v) = ((Active,) <$> (v .: "active_book"))
                     <|> ((Archived,) <$> (v .: "archived_book"))
                     >>= inner
    where inner (s, Object v') = Book <$> v' .: "id"
                                      <*> pure s
                                      <*> v' .: "version"
                                      <*> parseJSON (Object v')
                                      <*> v' .: "org"
          inner (_, _) = mempty
  parseJSON _ = mempty

data CreateBook = CreateBook OrgId BookBody deriving (Eq, Show)
instance Action CreateBook BookBody Book where
  toMethod = const methodPost
  toPathPieces (CreateBook oid _) = ["orgs", unOrgId oid, "books"]
  toBodyObject (CreateBook _ body) = Just body

data FetchBooks = FetchBooks OrgId deriving (Eq, Show)
instance Action FetchBooks Void [Book] where
  toPathPieces (FetchBooks oid) = ["orgs", unOrgId oid, "books"]

data FetchBook = FetchBook OrgId BookId deriving (Eq, Show)
instance Action FetchBook Void Book where
  toPathPieces (FetchBook oid bid) = ["orgs", unOrgId oid, "books", unBookId bid]

data PatchBook = PatchBook Book deriving (Eq, Show)
instance Action PatchBook BookBody Book where
  toMethod = const methodPatch
  toPathPieces (PatchBook book) = [ "orgs"
                                  , unOrgId $ bookOrgId book
                                  , "books"
                                  , unBookId $ bookId book
                                  ]
  toBodyObject (PatchBook book) = Just $ bookBody book

data ArchiveBook = ArchiveBook OrgId BookId deriving (Eq, Show)
instance Action ArchiveBook Void Book where
  toMethod = const methodPost
  toPathPieces (ArchiveBook oid bid) = ["orgs", unOrgId oid, "books", unBookId bid]

data ActivateBook = ActivateBook OrgId BookId deriving (Eq, Show)
instance Action ActivateBook Void Book where
  toMethod = const methodPost
  toPathPieces (ActivateBook oid bid) = ["orgs", unOrgId oid, "books", unBookId bid]
