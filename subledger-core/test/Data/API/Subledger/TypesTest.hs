{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.API.Subledger.TypesTest (suite) where

import Data.Aeson
import Data.API.Subledger.Instances
import Data.API.Subledger.Types
import Data.Maybe (fromJust)
import Data.Time.Calendar
import Data.Time.Clock
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.TH (testGroupGenerator)


suite :: TestTree
suite = $(testGroupGenerator)


case_decode_AccountingValue :: Assertion
case_decode_AccountingValue = do
  let json1 = "{\"type\": \"debit\", \"amount\": \"3.14\"}"
      value1 = AccountingDebitValue 3.14
  Just value1 @=? decode json1
  let json2 = "{\"type\": \"credit\", \"amount\": \".015\"}"
      value2 = AccountingCreditValue 0.015
  Just value2 @=? decode json2
  let json3 = "{\"type\": \"zero\", \"amount\": \"0\"}"
      value3 = AccountingZeroValue
  Just value3 @=? decode json3

case_encode_AccountingValue :: Assertion
case_encode_AccountingValue = do
  let value1 = AccountingZeroValue
      json1 = "{\"amount\":\"0.0\",\"type\":\"zero\"}"
  json1 @=? encode value1
  let value2 = AccountingCreditValue 0.015
      json2 = "{\"amount\":\"0.015\",\"type\":\"credit\"}"
  json2 @=? encode value2
  let value3 = AccountingDebitValue 3.14
      json3 = "{\"amount\":\"3.14\",\"type\":\"debit\"}"
  json3 @=? encode value3

prop_JSON_roundtrip_AccountingValue :: AccountingValue -> Bool
prop_JSON_roundtrip_AccountingValue a = decode (encode a) == Just a


case_decode_EffectiveAt :: Assertion
case_decode_EffectiveAt = do
  let json1 = "\"2016-04-19T17:14:53.000Z\""
  let effectiveAt1 = fromUTCTime $ UTCTime { utctDay = ModifiedJulianDay 57497
                                           , utctDayTime = secondsToDiffTime 62093
                                           }
  Just effectiveAt1 @=? decode json1

case_encode_EffectiveAt :: Assertion
case_encode_EffectiveAt = do
  let json1 = "\"2016-04-19T17:14:53.000Z\""
  let effectiveAt1 = fromUTCTime $ UTCTime { utctDay = ModifiedJulianDay 57497
                                           , utctDayTime = secondsToDiffTime 62093
                                           }
  json1 @=? encode effectiveAt1

prop_roundtrip_EffectiveAt :: EffectiveAt -> Bool
prop_roundtrip_EffectiveAt a = fromUTCTime (toUTCTime a) == a

prop_JSON_roundtrip_EffectiveAt :: EffectiveAt -> Bool
prop_JSON_roundtrip_EffectiveAt a = decode (encode a) == Just a
