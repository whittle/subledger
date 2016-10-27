{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Network.API.Subledger.Test.Book (spec) where

import Data.API.Subledger.Book
import Data.API.Subledger.Org
import Data.API.Subledger.Types
import Network.API.Subledger.Test.Prelude
import Test.Hspec

spec :: SubledgerSpec
spec subledger =
  describe "Book specs" $ do
    it "successfully creates a book" $ do
      Right org <- subledger $
        return =<< createOrg "sample book org"
      let oid = orgId org
      result <- subledger $ do
        b <- createBook oid $ BookBody "sample book" Nothing
        return b
      result `shouldSatisfy` isRight
      let Right Book { bookState = state
                     , bookOrgId = boid
                     , bookBody = body
                     } = result
      state `shouldBe` Active
      boid `shouldBe` oid
      body `shouldBe` BookBody { bookBodyDescription = "sample book"
                               , bookBodyReference = Nothing
                               }
    it "successfully retrieves a book" $ do
      Right org <- subledger $
        return =<< createOrg "another book org"
      let oid = orgId org
      result <- subledger $ do
        Book { bookId = bid } <-
          createBook oid $ BookBody "another book" Nothing
        return =<< fetchBook oid bid
      result `shouldSatisfy` isRight
