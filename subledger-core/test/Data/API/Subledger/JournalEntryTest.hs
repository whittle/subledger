{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}

module Data.API.Subledger.JournalEntryTest (suite) where

import Data.Aeson (decode)
import Data.API.Subledger.Account (AccountId(..))
import Data.API.Subledger.Book
import Data.API.Subledger.JournalEntry
import Data.API.Subledger.Types
import Data.Monoid ((<>))
import Data.Time.Calendar
import Data.Time.Clock
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)


suite :: TestTree
suite = $(testGroupGenerator)

case_decode_journal_entry :: Assertion
case_decode_journal_entry = do
  let json = "{\"posting_journal_entry\": {\"id\": \"foo\", "
             <> "\"book\": \"bar\", \"effective_at\": \"2016-04-19T17:14:53.000Z\", "
             <> "\"description\": \"a journal entry named foo\", "
             <> "\"reference\": \"http://ocho.co/baz\", \"version\": 42}}"
  let effectiveAt = fromUTCTime $ UTCTime { utctDay = ModifiedJulianDay 57497
                                          , utctDayTime = secondsToDiffTime 62093
                                          }
  let body = JournalEntryBody { journalEntryBodyDescription = "a journal entry named foo"
                              , journalEntryBodyReference = Just "http://ocho.co/baz"
                              , journalEntryBodyEffectiveAt = effectiveAt
                              }
  let journalEntry = JournalEntry { journalEntryId = JournalEntryId "foo"
                                  , journalEntryState = JEPosting
                                  , journalEntryVersion = 42
                                  , journalEntryBody = body
                                  , journalEntryBookId = BookId "bar"
                                  }
  Just journalEntry @=? decode json

case_decode_journal_entry_body :: Assertion
case_decode_journal_entry_body = do
  let json = "{\"description\": \"foo\", \"reference\": \"https://bar.org\", "
          <> "\"effective_at\": \"2016-05-24T21:39:36.000Z\"}"
  let effectiveAt = fromUTCTime $ UTCTime { utctDay = ModifiedJulianDay 57532
                                          , utctDayTime = secondsToDiffTime 77976
                                          }
  let body = JournalEntryBody { journalEntryBodyDescription = "foo"
                              , journalEntryBodyReference = Just "https://bar.org"
                              , journalEntryBodyEffectiveAt = effectiveAt
                              }
  Just body @=? decode json

case_decode_line_body :: Assertion
case_decode_line_body = do
  let json = "{\"value\":{\"amount\":\"0.5\",\"type\":\"debit\"},\"reference\":null,\"account\":\"h2pdVW1LJG5OL2mNMfXiSS\",\"description\":\"debit line\"}"
      body = LineBody { lineBodyAccount = AccountId "h2pdVW1LJG5OL2mNMfXiSS"
                      , lineBodyDescription = "debit line"
                      , lineBodyReference = Nothing
                      , lineBodyValue = AccountingDebitValue 0.5
                      }
  Just body @=? decode json
