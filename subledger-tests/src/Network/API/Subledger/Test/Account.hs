{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}

module Network.API.Subledger.Test.Account
       ( spec
       , establishAccount
       , establishTwoAccounts
       ) where

import Data.API.Subledger.Account
import Data.API.Subledger.Book
import Data.API.Subledger.Org
import Data.API.Subledger.Types
import Data.Time.Clock (getCurrentTime)
import Network.API.Subledger.Test.Book (establishBook)
import Network.API.Subledger.Test.Org (establishOrg)
import Network.API.Subledger.Test.Prelude
import Test.Hspec

spec :: SubledgerSpec
spec subledger =
  describe "account specs" $
    beforeWith (establishOrg subledger) $
    beforeWith (establishBook subledger) $ do
      it "successfully creates an account" $ \(oid, bid) -> do
        result <- subledger $
          return =<< (createAccount oid bid "sample account" CreditNormal -&- Reference "http://baz.quux")
        result `shouldSatisfy` isRight
        let Right Account { accountState = state
                          , accountBookId = abid
                          , accountBody = aBody
                          } = result
        state `shouldBe` Active
        abid `shouldBe` Just bid
        aBody `shouldBe` AccountBody "sample account" (Just $ Reference "http://baz.quux") CreditNormal
      context "with existing account" $
        beforeWith (establishAccount subledger) $ do
          it "successfully retrieves an account" $ \(oid, bid, aid) -> do
            result <- subledger $
              fetchAccount oid bid aid >>= return
            result `shouldSatisfy` isRight
          it "successfully retrieves the balance of that account" $ \(oid, bid, aid) -> do
            result <- subledger $ do
              t <- fmap fromUTCTime $ liftIO getCurrentTime
              fetchAccountBalance oid bid aid t >>= return
            result `shouldSatisfy` isRight

establishAccount :: SubledgerInterpreter -> (OrgId, BookId) -> IO (OrgId, BookId, AccountId)
establishAccount subledger (oid, bid) = do
  Right Account { accountId = aid } <-
    subledger $ return =<< createAccount oid bid "sample account" DebitNormal
  return (oid, bid, aid)

establishTwoAccounts :: SubledgerInterpreter -> (OrgId, BookId) -> IO (OrgId, BookId, AccountId, AccountId)
establishTwoAccounts subledger (oid, bid) = do
  Right Account { accountId = aid1 } <-
    subledger $ return =<<
      createAccount oid bid "sample account—debit" DebitNormal
  Right Account { accountId = aid2 } <-
    subledger $ return =<<
      createAccount oid bid "sample account—credit" CreditNormal
  return (oid, bid, aid1, aid2)
