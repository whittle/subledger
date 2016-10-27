{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Network.API.Subledger.Test.Account (spec) where

import Data.API.Subledger.Account
import Data.API.Subledger.Book
import Data.API.Subledger.Org
import Data.API.Subledger.Types
import Network.API.Subledger.Test.Prelude
import Test.Hspec

spec :: SubledgerSpec
spec subledger =
  describe "account specs" $ do
    it "successfully creates an account" $ do
      Right Org { orgId = oid } <-
        subledger $ return =<< createOrg "account spec org"
      Right Book { bookId = bid } <-
        subledger $ return =<< createBook oid (BookBody "account spec book" Nothing)
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
