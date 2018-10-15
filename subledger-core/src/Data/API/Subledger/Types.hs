{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.API.Subledger.Types
       ( AccountingValue(..)
       , Action(..)
       , BalanceValue(..)
       , CreditValue(..)
       , DebitValue(..)
       , WrappedBalanceValue(..)
       , EffectiveAt(..)
       , fromUTCTime
       , ResourceState(..)
       , Reference(..)
       , Void
       ) where

import           Prelude hiding (print)
import           Control.Arrow ((***))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Char8 as B
import qualified Data.Scientific as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Textual
import           Data.Time.Clock (DiffTime, UTCTime(..))
import           Data.Time.ISO8601 (parseISO8601, formatISO8601Millis)
import           Data.Typeable (Typeable)
import qualified Data.Void as V
import           GHC.Generics (Generic)
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types.Method as M


-- ResourceState
data ResourceState
  = Active
  | Archived
  deriving (Bounded, Enum, Eq, Show)


-- EffectiveAt
newtype EffectiveAt = EffectiveAt { toUTCTime :: UTCTime }
                      deriving (Eq, Show)

instance A.FromJSON EffectiveAt where
  parseJSON = A.withText "ISO8601" (outer . parseISO8601 . T.unpack)
    where outer = maybe mempty (pure . EffectiveAt)

instance Printable EffectiveAt where
  print = print . formatISO8601Millis . toUTCTime

instance A.ToJSON EffectiveAt where
  toJSON = A.String . toText

fromUTCTime :: UTCTime -> EffectiveAt
fromUTCTime = EffectiveAt . roundUTCTimeToMillis

roundUTCTimeToMillis :: UTCTime -> UTCTime
roundUTCTimeToMillis (UTCTime day dayTime) =
  UTCTime day $ roundDiffToMillis dayTime
  where roundDiffToMillis = (/1000) . fromInt . truncate . (*1000)
        fromInt = fromIntegral :: Int -> DiffTime


-- AccountingAmount
newtype AccountingAmount = AccountingAmount { toScientific :: S.Scientific }
                         deriving (Eq, Show)

instance A.FromJSON AccountingAmount where
  parseJSON = A.withText "numeric string" (pure . AccountingAmount . read . reqZero . T.uncons)
    where reqZero (Just ('.', t)) = '0':'.':(T.unpack t)
          reqZero (Just (c, t)) = c:(T.unpack t)
          reqZero Nothing = error "empty string for amount"

instance A.ToJSON AccountingAmount where
  toJSON = A.String . T.pack . S.formatScientific S.Fixed Nothing . toScientific


-- | AccountingValue
data AccountingValue = AccountingDebitValue S.Scientific
                     | AccountingCreditValue S.Scientific
                     | AccountingZeroValue
                     deriving (Eq, Generic, Show)

instance A.FromJSON AccountingValue where
  parseJSON = A.withObject "value object" $ \v -> do
    t <- v A..: "type" :: A.Parser Text
    case t of
     "debit" -> AccountingDebitValue . toScientific <$> v A..: "amount"
     "credit"-> AccountingCreditValue . toScientific <$> v A..: "amount"
     "zero"  -> pure AccountingZeroValue
     _       -> error "Accounting values must have a type of debit, credit, or zero."

valueObject :: Text -> S.Scientific -> A.Value
valueObject t a = A.object [ "type" A..= A.String t
                           , "amount" A..= AccountingAmount a
                           ]

instance A.ToJSON AccountingValue where
  toJSON (AccountingDebitValue s) = valueObject "debit" s
  toJSON (AccountingCreditValue s) = valueObject "credit" s
  toJSON AccountingZeroValue = valueObject "zero" 0


-- | BalanceValue
data BalanceValue = BalanceValue DebitValue CreditValue AccountingValue
                  deriving (Eq, Show)

instance A.FromJSON BalanceValue where
  parseJSON = A.withObject "balance value object" $ \v ->
    BalanceValue <$> v A..: "debit_value"
                 <*> v A..: "credit_value"
                 <*> v A..: "value"

instance A.ToJSON BalanceValue where
  toJSON (BalanceValue d c a) = A.object
    [ "debit_value" A..= d
    , "credit_value" A..= c
    , "value" A..= a
    ]


-- | DebitValue
data DebitValue = DebitValue S.Scientific
                | DebitZeroValue
                deriving (Eq, Show)

instance A.FromJSON DebitValue where
  parseJSON = A.withObject "debit value" $ \v -> do
    t <- v A..: "type" :: A.Parser Text
    case t of
      "debit" -> DebitValue . toScientific <$> v A..: "amount"
      "zero" -> pure DebitZeroValue
      _ -> error "Debit values must have a type of debit or zero."

instance A.ToJSON DebitValue where
  toJSON (DebitValue s) = valueObject "debit" s
  toJSON DebitZeroValue = valueObject "zero" 0


-- | CreditValue
data CreditValue = CreditValue S.Scientific
                 | CreditZeroValue
                 deriving (Eq, Show)

instance A.FromJSON CreditValue where
  parseJSON = A.withObject "credit value" $ \v -> do
    t <- v A..: "type" :: A.Parser Text
    case t of
      "credit" -> CreditValue . toScientific <$> v A..: "amount"
      "zero" -> pure CreditZeroValue
      _ -> error "Credit values must have a type of credit or zero."

instance A.ToJSON CreditValue where
  toJSON (CreditValue s) = valueObject "credit" s
  toJSON CreditZeroValue = valueObject "zero" 0


-- | WrappedBalanceValue
newtype WrappedBalanceValue = WrappedBalanceValue
  { unwrapBalanceValue :: BalanceValue }
  deriving (Eq, Show)

instance A.FromJSON WrappedBalanceValue where
  parseJSON = A.withObject "wrapped balance value objecT" $ \v ->
    WrappedBalanceValue <$> v A..: "balance"

instance A.ToJSON WrappedBalanceValue where
  toJSON (WrappedBalanceValue b) = A.object ["balance" A..= b]


-- | Reference
newtype Reference = Reference
                    { uri :: Text
                    } deriving (Eq, Show, Typeable)

instance A.FromJSON Reference where
  parseJSON = A.withText "Data.API.Subledger.Types.Reference" (return . Reference)

instance A.ToJSON Reference where
  toJSON = A.String . uri


-- Action
class A.ToJSON a => Action q a r | q -> a r where
  toMethod :: q -> M.Method
  toMethod = const M.methodGet

  toPathPieces :: q -> [Text]

  toPath :: q -> B.ByteString
  toPath = B.concat . ("v2":) . map (B.cons '/' . encodeUtf8) . toPathPieces

  toQueryParams :: q -> [(Text, Maybe Text)]
  toQueryParams = const []

  toQueryString :: q -> [(B.ByteString, Maybe B.ByteString)]
  toQueryString = map (encodeUtf8 *** (encodeUtf8 <$>)) . toQueryParams

  toBodyObject :: A.ToJSON a => q -> Maybe a
  toBodyObject = const Nothing

  toBody :: q -> HTTP.RequestBody
  toBody = maybe (HTTP.RequestBodyBS "") (HTTP.RequestBodyLBS . A.encode) . toBodyObject

  toRequest :: q -> HTTP.Request
  toRequest q = HTTP.setQueryString (toQueryString q) $ HTTP.defaultRequest
    { HTTP.method = toMethod q
    , HTTP.path = toPath q
    , HTTP.requestBody = toBody q
    }


-- Void
newtype Void = Void V.Void deriving (Eq, Show)

instance A.ToJSON Void where
  toJSON = error "the void is uninhabited"
