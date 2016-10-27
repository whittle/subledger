{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}

module Network.API.Subledger.Test.Org
       ( spec
       , establishOrg
       ) where

import Data.API.Subledger.Org
import Data.API.Subledger.Types
import Network.API.Subledger.Test.Prelude
import Test.Hspec

spec :: SubledgerSpec
spec subledger =
  describe "Org specs" $ do
    it "successfully creates an org" $ do
      result <- subledger $ do
        return =<< createOrg "Sample Org"
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
        return =<< fetchOrg oid
      result `shouldSatisfy` isRight
      let Right Org { orgState = state
                    , orgBody = body
                    } = result
      state `shouldBe` Active
      body `shouldBe` OrgBody { orgBodyDescription = Just "Another org"
                              , orgBodyReference = Nothing
                              }

establishOrg :: SubledgerInterpreter -> () -> IO OrgId
establishOrg subledger _ = do
  Right Org { orgId = oid } <-
    subledger $ return =<< createOrg "account spec org"
  return oid
