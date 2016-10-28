{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}

module Data.API.Subledger.OrgTest (suite) where

import Data.Aeson (decode)
import Data.Monoid ((<>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)

import Data.API.Subledger.Org
import Data.API.Subledger.Types

suite :: TestTree
suite = $(testGroupGenerator)

case_decode_org :: Assertion
case_decode_org = do
  let json = "{\"active_org\": {\"id\": \"foo\", \"version\": 0}}"
  let org = Org { orgId = OrgId "foo"
                , orgState = Active
                , orgBody = OrgBody { orgBodyDescription = Nothing
                                    , orgBodyReference = Nothing
                                    }
                , orgVersion = 0
                }
  Just org @=? decode json
  let json' = "{\"archived_org\": {\"id\": \"bar\", \"version\": 1}}"
  let org' = Org { orgId = OrgId "bar"
                 , orgState = Archived
                 , orgBody = OrgBody { orgBodyDescription = Nothing
                                     , orgBodyReference = Nothing
                                     }
                 , orgVersion = 1
                 }
  Just org' @=? decode json'
  let json'' = "{\"active_org\": {\"id\": \"baz\", \"description\": \"desc of baz\", "
             <> "\"reference\": \"http://baz.com/foo\", \"version\": 2}}"
  let org'' = Org { orgId = OrgId "baz"
                  , orgState = Active
                  , orgBody = OrgBody { orgBodyDescription = Just "desc of baz"
                                      , orgBodyReference = Just $ Reference "http://baz.com/foo"
                                      }
                  , orgVersion = 2
                  }
  Just org'' @=? decode json''
