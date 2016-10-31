{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Data.API.Subledger.JournalEntry
       ( JournalEntryId(..)
       , JournalEntryBody(..)
       , JournalEntryState(..)
       , JournalEntry(..)
       , LineId(..)
       , LineBody(..)
       , Line(..)
       , createAndPostJournalEntry
       ) where

import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Aeson.Types (emptyArray, Options(..))
import           Data.API.Subledger.Account (AccountId(..))
import           Data.API.Subledger.Book (BookId(..))
import           Data.API.Subledger.Org (OrgId(..))
import           Data.API.Subledger.Request
import           Data.API.Subledger.Types
import           Data.API.Subledger.Util
import           Data.HashMap.Strict (adjust)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Vector (singleton)
import           GHC.Generics (Generic)


newtype JournalEntryId = JournalEntryId { unJournalEntryId :: Text }
                       deriving (Eq, Show)

instance FromJSON JournalEntryId where
  parseJSON (String s) = pure $ JournalEntryId s
  parseJSON _ = mempty

data JournalEntryBody = JournalEntryBody { journalEntryBodyDescription :: Text
                                         , journalEntryBodyReference :: Maybe Text
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


newtype LineId = LineId { unLineId :: Text }
               deriving (Eq, Show)

instance FromJSON LineId where
  parseJSON (String s) = pure $ LineId s
  parseJSON _ = mempty

data Line = Line { lineId :: LineId
                 , lineJournalEntryId :: JournalEntryId
                 , lineEffectiveAt :: EffectiveAt
                 , lineBody :: LineBody
                 , lineOrder :: Int
                 , lineBalance :: Maybe BalanceValue
                 , lineVersion :: Int
                 } deriving (Eq, Show)

data LineBody = LineBody { lineBodyAccount :: AccountId
                         , lineBodyDescription :: Text
                         , lineBodyReference :: Maybe Text
                         , lineBodyValue :: AccountingValue
                         } deriving (Eq, Generic, Show)

lineBodyFields :: String -> String
lineBodyFields = snakeCase . drop 8

instance FromJSON LineBody where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = lineBodyFields }

instance ToJSON LineBody where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = lineBodyFields }


data CreateAndPostJournalEntry
type instance SubledgerReturn CreateAndPostJournalEntry = JournalEntry

createAndPostJournalEntry :: OrgId
                          -> BookId
                          -> EffectiveAt
                          -> Text -- ^ description
                          -> SubledgerRequest CreateAndPostJournalEntry
createAndPostJournalEntry (OrgId oid) (BookId bid) at desc =
  mkRequest POST
            ["orgs", oid, "books", bid, "journal_entries", "create_and_post"]
            [ ("effective_at", toJSON at)
            , ("description", toJSON desc)
            , ("lines", emptyArray)
            ]

instance HasParam CreateAndPostJournalEntry LineBody where
  addParam l = adjustBodyObject $ adjust f "lines"
    where f (Array a) = Array $ a `mappend` singleton (toJSON l)
