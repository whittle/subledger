{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.API.Subledger.JournalEntry
       ( JournalEntryId(..)
       , JournalEntryBody(..)
       , JournalEntryState(..)
       , JournalEntry(..)
       ) where

import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Aeson.Types (Options(..))
import qualified Data.Text as T
import           GHC.Generics (Generic)

import           Data.API.Subledger.Book (BookId(..))
import           Data.API.Subledger.Types
import           Data.API.Subledger.Util

newtype JournalEntryId = JournalEntryId { unJournalEntryId :: T.Text }
                       deriving (Eq, Show)

instance FromJSON JournalEntryId where
  parseJSON (String s) = pure $ JournalEntryId s
  parseJSON _ = mempty

data JournalEntryBody = JournalEntryBody { journalEntryBodyDescription :: T.Text
                                         , journalEntryBodyReference :: Maybe T.Text
                                         , journalEntryBodyEffectiveAt :: EffectiveAt
                                         } deriving (Eq, Generic, Show)

journalEntryBodyFields :: String -> String
journalEntryBodyFields = snakeCase . drop 16

instance FromJSON JournalEntryBody where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = journalEntryBodyFields }

instance ToJSON JournalEntryBody where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = journalEntryBodyFields }

data JournalEntryState = JEActive | JEArchived | JEPosting | JEPosted deriving (Eq, Show)

data JournalEntry = JournalEntry { journalEntryId :: JournalEntryId
                                 , journalEntryState :: JournalEntryState
                                 , journalEntryVersion :: Int
                                 , journalEntryBody :: JournalEntryBody
                                 , journalEntryBookId :: BookId
                                 } deriving (Eq, Show)

instance FromJSON JournalEntry where
  parseJSON (Object v) = ((JEActive,) <$> (v .: "active_journal_entry"))
                         <|> ((JEArchived,) <$> (v .: "archived_journal_entry"))
                         <|> ((JEPosting,) <$> (v .: "posting_journal_entry"))
                         <|> ((JEPosted,) <$> (v .: "posted_journal_entry"))
                         >>= inner
    where inner (s, Object v') = JournalEntry <$> v' .: "id"
                                              <*> pure s
                                              <*> v' .: "version"
                                              <*> parseJSON (Object v')
                                              <*> v' .: "book"
          inner _ = mempty
  parseJSON _ = mempty
