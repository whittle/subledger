module Data.API.Subledger.Line
       ( Line(..)
       , LineBody(..)
       , LineId(..)
       ) where

import           Data.Aeson
import qualified Data.Text as T

import           Data.API.Subledger.Account (AccountId(..))
import           Data.API.Subledger.JournalEntry (JournalEntryId(..))
import           Data.API.Subledger.Types

newtype LineId = LineId { unLineId :: T.Text }
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

data LineBody = LineBody { lineBodyAccountId :: AccountId
                         , lineBodyDescription :: T.Text
                         , lineBodyReference :: Maybe T.Text
                         , lineBodyValue :: AccountingValue
                         } deriving (Eq, Show)

-- instance FromJSON Line where
--   parseJSON (Object v) = do
--     lid <- v .: "id"
--     jid <- v .: "journal_entry"
--     eff <- v .: "effective_at"
--     aid <- v .: "account"
--     des <- v .: "description"
--     ref <- v .:? "reference"
--     val <- v .: "value"
--     ord <- v .: "order"
--     bal <- v .:? "balance"
--     ver <- v .: "version"
--     return Line { lineId = LineId lid
--                 , lineJournalEntryId = JournalEntryId jid
--                 , lineEffectiveAt = eff
--                 , lineBody = LineBody { lineBodyAccountId = AccountId aid
--                                       , lineBodyDescription = des
--                                       , lineBodyReference = ref
--                                       , lineBodyValue = val
--                                       }
--                 , lineOrder = ord
--                 , lineBalance = bal
--                 , lineVersion = ver
--                 }
--   parseJSON _ = mempty
