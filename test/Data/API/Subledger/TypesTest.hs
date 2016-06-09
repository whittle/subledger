{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -XTemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.API.Subledger.TypesTest (suite) where

import Data.Aeson
import Data.Maybe (fromJust)
import Data.Time.Calendar
import Data.Time.Clock
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.TH (testGroupGenerator)

import Data.API.Subledger.Types

suite :: TestTree
suite = $(testGroupGenerator)

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

instance Arbitrary EffectiveAt where
  arbitrary = fromUTCTime <$> arbitrary
  shrink = fmap fromUTCTime . shrink . toUTCTime

prop_roundtrip_EffectiveAt :: EffectiveAt -> Bool
prop_roundtrip_EffectiveAt a = roundtrip a == a
  where roundtrip = fromUTCTime . toUTCTime

prop_JSON_roundtrip_EffectiveAt :: EffectiveAt -> Bool
prop_JSON_roundtrip_EffectiveAt a = roundtrip a == a
  where roundtrip = fromJust . decode . encode
