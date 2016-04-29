{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}

module Data.API.Subledger.BookTest (suite) where

import Data.Aeson (decode)
import Data.Monoid ((<>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)

import Data.API.Subledger.Book
import Data.API.Subledger.Org
import Data.API.Subledger.Types

suite :: TestTree
suite = $(testGroupGenerator)

case_decode_book :: Assertion
case_decode_book = do
  let json = "{\"active_book\": {\"id\": \"foo\", "
             <> "\"org\": \"bar\", \"description\": \"blargh\", "
             <> "\"reference\": \"http://sample.org/nonsense\", \"version\": 0}}"
  let book = Book { bookId = BookId "foo"
                  , bookState = Active
                  , bookVersion = 0
                  , bookBody = BookBody { bookDescription = "blargh"
                                        , bookReference = Just "http://sample.org/nonsense"
                                        }
                  , bookOrgId = OrgId "bar"
                  }
  Just book @=? decode json
