{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Network.API.Subledger.Test.Org (spec) where

import Data.API.Subledger.Org
import Data.API.Subledger.Types
import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..))
import Network.API.Subledger.Test.Prelude
import Test.Hspec

spec :: SubledgerSpec
spec subledger = do
  describe "Org specs" $ do
    it "successfully creates an org" $ do
      result <- subledger $ do
        o <- createOrg "Sample Org"
        return o
      result `shouldSatisfy` isRight
      let Right Org { orgState = state
                    , orgBody = body
                    } = result
      state `shouldBe` Active
      body `shouldBe` OrgBody { orgBodyDescription = Just "Sample Org"
                              , orgBodyReference = Nothing
                              }
    it "successfully retrieves an org" $ do
      result <- subledger $ do
        Org { orgId = oid } <-
          createOrg "Another org"
        o <- fetchOrg oid
        return o
      result `shouldSatisfy` isRight
      let Right Org { orgState = state
                    , orgBody = body
                    } = result
      state `shouldBe` Active
      body `shouldBe` OrgBody { orgBodyDescription = Just "Another org"
                              , orgBodyReference = Nothing
                              }
