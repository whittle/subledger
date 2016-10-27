{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}

module Network.API.Subledger.Test.Account
       ( spec
       , establishAccount
       ) where

import Data.API.Subledger.Account
import Data.API.Subledger.Book
import Data.API.Subledger.Error
import Data.API.Subledger.Org
import Data.API.Subledger.Types
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
        let body = AccountBody "sample account" Nothing CreditNormal
        result <- subledger $
          return =<< createAccount oid bid body
        result `shouldSatisfy` isRight
        let Right Account { accountState = state
                          , accountBookId = abid
                          , accountBody = aBody
                          } = result
        state `shouldBe` Active
        abid `shouldBe` Just bid
        aBody `shouldBe` body
      beforeWith (establishAccount subledger) $
        it "successfully retrieves an account" $ \(oid, bid, aid) -> do
          result <- subledger $ do
            return =<< fetchAccount oid bid aid
          result `shouldSatisfy` isRight

establishAccount :: SubledgerInterpreter -> (OrgId, BookId) -> IO (OrgId, BookId, AccountId)
establishAccount subledger (oid, bid) = do
  let body = AccountBody "sample account" Nothing DebitNormal
  Right Account { accountId = aid } <-
    subledger $ return =<< createAccount oid bid body
  return (oid, bid, aid)
