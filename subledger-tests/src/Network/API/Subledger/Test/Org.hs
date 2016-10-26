{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Network.API.Subledger.Test.Org (spec) where

import Data.API.Subledger.Org
import Data.Either (isRight)
import Network.API.Subledger.Test.Prelude
import Test.Hspec

spec :: SubledgerSpec
spec subledger = do
  describe "Org specs" $ do
    it "successfully creates an org" $ do
      result <- subledger $ do
        o <- createOrg "Sample Org"
        -- get rid of org?
        return o
      result `shouldSatisfy` isRight
