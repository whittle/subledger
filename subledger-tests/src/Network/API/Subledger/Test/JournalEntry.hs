{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}

module Network.API.Subledger.Test.JournalEntry
       ( spec
       , establishJournalEntry
       ) where

import Prelude (fromInteger, fromRational)
import Data.API.Subledger.Account (AccountId)
import Data.API.Subledger.Book (BookId)
import Data.API.Subledger.JournalEntry
import Data.API.Subledger.Org (OrgId)
import Data.API.Subledger.Types
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Network.API.Subledger.Test.Account (establishTwoAccounts)
import Network.API.Subledger.Test.Book (establishBook)
import Network.API.Subledger.Test.Org (establishOrg)
import Network.API.Subledger.Test.Prelude
import Test.Hspec

spec :: SubledgerSpec
spec subledger =
  describe "journal entry specs" $
    beforeWith (establishOrg subledger) $
    beforeWith (establishBook subledger) $
    beforeWith (establishTwoAccounts subledger) $
      it "successfully creates a journal entry" $ \(oid, bid, aid1, aid2) -> do
        let at = fromUTCTime $ UTCTime { utctDay = ModifiedJulianDay 57497
                                       , utctDayTime = secondsToDiffTime 62093
                                       }
        result <- subledger $ do
          let a = createAndPostJournalEntry oid bid at "sample journal entry"
              a' = a -&- LineBody aid1 "debit line" Nothing (AccountingDebitValue 0.5)
              a'' = a' -&- LineBody aid2 "credit line" Nothing (AccountingCreditValue 0.5)
          r <- a''
          return r
        result `shouldSatisfy` isRight
        let Right JournalEntry { journalEntryState = state
                               , journalEntryBookId = bid'
                               , journalEntryBody = body
                               } = result
        state `shouldBe` JEPosting
        bid' `shouldBe` bid
        body `shouldBe` JournalEntryBody "sample journal entry" Nothing at

establishJournalEntry :: SubledgerInterpreter
                      -> (OrgId, BookId, AccountId, AccountId)
                      -> IO (OrgId, BookId, AccountId, AccountId, JournalEntryId)
establishJournalEntry subledger (oid, bid, aid1, aid2) = do
  let at = fromUTCTime $ UTCTime { utctDay = ModifiedJulianDay 57497
                                 , utctDayTime = secondsToDiffTime 62093
                                 }
  Right JournalEntry { journalEntryId = jid } <-
    subledger $ return =<<
      createAndPostJournalEntry oid bid at "sample journal entry"
  return (oid, bid, aid1, aid2, jid)
