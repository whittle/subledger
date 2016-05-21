{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.API.Subledger.JournalEntry
       ( JournalEntryId(..)
       , JournalEntryBody(..)
       , JournalEntryState(..)
       , JournalEntry(..)
       ) where

import           Control.Applicative ((<|>), empty)
import           Data.Aeson
import qualified Data.Text as T

import           Data.API.Subledger.Book (BookId(..))
import           Data.API.Subledger.Types

newtype JournalEntryId = JournalEntryId { unJournalEntryId :: T.Text }
                       deriving (Eq, Show)

data JournalEntryBody = JournalEntryBody { journalEntryDescription :: T.Text
                                         , journalEntryReference :: Maybe T.Text
                                         , journalEntryEffectiveAt :: EffectiveAt
                                         } deriving (Eq, Show)

data JournalEntryState = JEActive | JEArchived | JEPosting | JEPosted deriving (Eq, Show)

data JournalEntry = JournalEntry { journalEntryId :: JournalEntryId
                                 , journalEntryState :: JournalEntryState
                                 , journalEntryVersion :: Int
                                 , journalEntryBody :: JournalEntryBody
                                 , journalEntryBookId :: BookId
                                 } deriving (Eq, Show)

mkJournalEntry :: JournalEntryState
                  -> T.Text
                  -> Int
                  -> T.Text
                  -> Maybe T.Text
                  -> EffectiveAt
                  -> T.Text
                  -> JournalEntry
mkJournalEntry s i v d r e b = JournalEntry { journalEntryId = JournalEntryId i
                                            , journalEntryState = s
                                            , journalEntryVersion = v
                                            , journalEntryBody = body
                                            , journalEntryBookId = BookId b
                                            }
  where body = JournalEntryBody { journalEntryDescription = d
                                , journalEntryReference = r
                                , journalEntryEffectiveAt = e
                                }

instance FromJSON JournalEntry where
  parseJSON (Object v) = ((JEActive,) <$> (v .: "active_journal_entry"))
                         <|> ((JEArchived,) <$> (v .: "archived_journal_entry"))
                         <|> ((JEPosting,) <$> (v .: "posting_journal_entry"))
                         <|> ((JEPosted,) <$> (v .: "posted_journal_entry"))
                         >>= inner
    where inner (s, Object v') = mkJournalEntry s
                                 <$> v' .: "id"
                                 <*> v' .: "version"
                                 <*> v' .: "description"
                                 <*> v' .:? "reference"
                                 <*> v' .: "effective_at"
                                 <*> v' .: "book"
          inner _ = empty
  parseJSON _ = empty
