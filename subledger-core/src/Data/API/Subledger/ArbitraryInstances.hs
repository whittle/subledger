{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.API.Subledger.ArbitraryInstances where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (oneof)
import Test.QuickCheck.Instances ()

import Data.API.Subledger.Types

instance Arbitrary AccountingValue where
  arbitrary = oneof [ AccountingDebitValue <$> arbitrary
                    , AccountingCreditValue <$> arbitrary
                    , pure AccountingZeroValue
                    ]
  shrink a@(AccountingDebitValue _) = AccountingZeroValue:(genericShrink a)
  shrink a@(AccountingCreditValue _) = AccountingZeroValue:(genericShrink a)
  shrink AccountingZeroValue = []
