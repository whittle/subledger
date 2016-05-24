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
                        , accountBody = AccountBody { accountBodyDescription = "this is account foo"
                                                    , accountBodyReference = Just "http://foo.co/bar"
                                                    , accountBodyNormalBalance = Credit
                                                    }
                        , accountBookId = Just $ BookId "bar"
                        }
  Just account @=? decode json

case_decode_account_body :: Assertion
case_decode_account_body = do
  let json = "{\"description\": \"foo\", \"reference\": \"https://bar.org\", "
          <> "\"normal_balance\": \"debit\"}"
  let body = AccountBody { accountBodyDescription = "foo"
                         , accountBodyReference = Just "https://bar.org"
                         , accountBodyNormalBalance = Debit
                         }
  Just body @=? decode json
