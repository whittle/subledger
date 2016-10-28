{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Data.API.Subledger.Book
       ( BookId(..)
       , BookBody(..)
       , Book(..)
       , createBook
       , fetchBook
       ) where

import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Aeson.Types (Options(..))
import           Data.API.Subledger.Org (OrgId(..))
import           Data.API.Subledger.Request
import           Data.API.Subledger.Types
import           Data.API.Subledger.Util
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Network.HTTP.Types.Method (methodPatch, methodPost)

newtype BookId = BookId { unBookId :: Text }
               deriving (Eq, Show)

instance FromJSON BookId where
  parseJSON (String s) = pure $ BookId s
  parseJSON _ = mempty

data BookBody = BookBody { bookBodyDescription :: Text
                         , bookBodyReference :: Maybe Reference
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

data CreateBook
type instance SubledgerReturn CreateBook = Book

createBook :: OrgId
           -> Text -- ^ description
           -> SubledgerRequest CreateBook
createBook oid s = mkRequest POST
                             ["orgs", unOrgId oid, "books"]
                             [("description", String s)]

data FetchBooks = FetchBooks OrgId deriving (Eq, Show)
instance Action FetchBooks Void [Book] where
  toPathPieces (FetchBooks oid) = ["orgs", unOrgId oid, "books"]

data FetchBook
type instance SubledgerReturn FetchBook = Book

fetchBook :: OrgId -> BookId -> SubledgerRequest FetchBook
fetchBook (OrgId oid) (BookId bid) = mkEmptyRequest ["orgs", oid, "books", bid]

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
