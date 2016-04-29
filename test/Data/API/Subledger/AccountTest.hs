{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}

module Data.API.Subledger.AccountTest (suite) where

import Data.Aeson (decode)
import Data.Monoid ((<>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)

import Data.API.Subledger.Account
import Data.API.Subledger.Book
import Data.API.Subledger.Types

suite :: TestTree
suite = $(testGroupGenerator)

case_decode_account :: Assertion
case_decode_account = do
  let json = "{\"active_account\": {\"id\": \"foo\", \"book\": \"bar\", "
             <> "\"description\": \"this is account foo\", \"reference\": \"http://foo.co/bar\", "
             <> "\"normal_balance\": \"credit\", \"version\": 1}}"
  let account = Account { accountId = AccountId "foo"
                        , accountState = Active
                        , accountVersion = 1
                        , accountBody = AccountBody { accountDescription = "this is account foo"
                                                    , accountReference = Just "http://foo.co/bar"
                                                    , accountNormalBalance = Credit
                                                    }
                        , accountBookId = Just $ BookId "bar"
                        }
  Just account @=? decode json
