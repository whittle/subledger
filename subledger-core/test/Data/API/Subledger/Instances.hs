{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.API.Subledger.Instances () where

import           Data.API.Subledger.Account (AccountId(..))
import           Data.API.Subledger.JournalEntry (JournalEntryBody(..), LineBody(..))
import           Data.API.Subledger.Types (AccountingValue(..), EffectiveAt(..), fromUTCTime)
import           Test.QuickCheck
import           Test.QuickCheck.Instances()


instance Arbitrary AccountId where
  arbitrary = AccountId <$> arbitrary
  shrink (AccountId i) = [AccountId i' | i' <- shrink i]

instance Arbitrary AccountingValue where
  arbitrary = oneof [ AccountingDebitValue <$> arbitrary
                    , AccountingCreditValue <$> arbitrary
                    , pure AccountingZeroValue
                    ]
  shrink (AccountingDebitValue s) = fmap AccountingDebitValue $ shrink s
  shrink (AccountingCreditValue s) = fmap AccountingCreditValue $ shrink s
  shrink AccountingZeroValue = []

instance Arbitrary EffectiveAt where
  arbitrary = fromUTCTime <$> arbitrary
  shrink = fmap fromUTCTime . shrink . toUTCTime

instance Arbitrary JournalEntryBody where
  arbitrary = JournalEntryBody <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LineBody where
  arbitrary = LineBody <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (LineBody a d r v) =
    [LineBody a' d' r' v' | (a', d', r', v') <- shrink (a, d, r, v)]
