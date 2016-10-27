{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}

module Network.API.Subledger.Test.Book
       ( spec
       , establishBook
       ) where

import Data.API.Subledger.Book
import Data.API.Subledger.Org
import Data.API.Subledger.Types
import Network.API.Subledger.Test.Org (establishOrg)
import Network.API.Subledger.Test.Prelude
import Test.Hspec

spec :: SubledgerSpec
spec subledger =
  describe "Book specs" $
    beforeWith (establishOrg subledger) $ do
      it "successfully creates a book" $ \oid -> do
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
      it "successfully retrieves a book" $ \oid -> do
        result <- subledger $ do
          Book { bookId = bid } <-
            createBook oid $ BookBody "another book" Nothing
          return =<< fetchBook oid bid
        result `shouldSatisfy` isRight

establishBook :: SubledgerInterpreter -> OrgId -> IO (OrgId, BookId)
establishBook subledger oid = do
  Right Book { bookId = bid } <-
    subledger $ return =<< createBook oid (BookBody "account spec book" Nothing)
  return (oid, bid)
