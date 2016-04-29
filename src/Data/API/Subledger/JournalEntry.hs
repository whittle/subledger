{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.API.Subledger.JournalEntry
       ( JournalEntryId(..)
       , EffectiveAt(..)
       , JournalEntryBody(..)
       , JournalEntryState(..)
       , JournalEntry(..)
       ) where

import           Control.Applicative ((<|>), empty)
import           Data.Aeson
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.ISO8601 (parseISO8601)

import           Data.API.Subledger.Book (BookId(..))

newtype JournalEntryId = JournalEntryId { unJournalEntryId :: T.Text }
                       deriving (Eq, Show)

newtype EffectiveAt = EffectiveAt { unEffectiveAt :: UTCTime }
                      deriving (Eq, Show)

instance FromJSON EffectiveAt where
  parseJSON (String s) = outer . parseISO8601 $ T.unpack s
    where outer (Just t) = pure $ EffectiveAt t
          outer _ = empty
  parseJSON _ = empty

data JournalEntryBody = JournalEntryBody { journalEntryDescription :: T.Text
                                         , journalEntryReference :: Maybe T.Text
                                         , journalEntryEffectiveAt :: EffectiveAt
                                         } deriving (Eq, Show)

data JournalEntryState = Active | Archived | Posting | Posted deriving (Eq, Show)

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
  parseJSON (Object v) = ((Active,) <$> (v .: "active_journal_entry"))
                         <|> ((Archived,) <$> (v .: "archived_journal_entry"))
                         <|> ((Posting,) <$> (v .: "posting_journal_entry"))
                         <|> ((Posted,) <$> (v .: "posted_journal_entry"))
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
