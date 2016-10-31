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
        return =<< (createOrg "Sample Org" -&- Reference "http://foo.bar")
      result `shouldSatisfy` isRight
      let Right Org { orgState = state
                    , orgBody = oBody
                    } = result
      state `shouldBe` Active
      oBody `shouldBe` OrgBody { orgBodyDescription = Just "Sample Org"
                               , orgBodyReference = Just $ Reference "http://foo.bar"
                               }
    it "successfully retrieves an org" $ do
      result <- subledger $ do
        Org { orgId = oid } <-
          createOrg "Another org"
        return =<< fetchOrg oid
      result `shouldSatisfy` isRight
      let Right Org { orgState = state
                    , orgBody = oBody
                    } = result
      state `shouldBe` Active
      oBody `shouldBe` OrgBody { orgBodyDescription = Just "Another org"
                               , orgBodyReference = Nothing
                               }

establishOrg :: SubledgerInterpreter -> () -> IO OrgId
establishOrg subledger _ = do
  Right Org { orgId = oid } <-
    subledger $ return =<< createOrg "account spec org"
  return oid
