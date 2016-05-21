{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}

module Data.API.Subledger.JournalEntryTest (suite) where

import Data.Aeson (decode)
import Data.Monoid ((<>))
import Data.Time.Calendar
import Data.Time.Clock
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)

import Data.API.Subledger.Book
import Data.API.Subledger.JournalEntry
import Data.API.Subledger.Types

suite :: TestTree
suite = $(testGroupGenerator)

case_decode_journal_entry :: Assertion
case_decode_journal_entry = do
  let json = "{\"posting_journal_entry\": {\"id\": \"foo\", "
             <> "\"book\": \"bar\", \"effective_at\": \"2016-04-19T17:14:53.000Z\", "
             <> "\"description\": \"a journal entry named foo\", "
             <> "\"reference\": \"http://ocho.co/baz\", \"version\": 42}}"
  let effectiveAt = EffectiveAt $ UTCTime { utctDay = ModifiedJulianDay 57497
                                          , utctDayTime = secondsToDiffTime 62093
                                          }
  let body = JournalEntryBody { journalEntryDescription = "a journal entry named foo"
                              , journalEntryReference = Just "http://ocho.co/baz"
                              , journalEntryEffectiveAt = effectiveAt
                              }
  let journalEntry = JournalEntry { journalEntryId = JournalEntryId "foo"
                                  , journalEntryState = JEPosting
                                  , journalEntryVersion = 42
                                  , journalEntryBody = body
                                  , journalEntryBookId = BookId "bar"
                                  }
  Just journalEntry @=? decode json
