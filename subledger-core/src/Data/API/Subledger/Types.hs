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
       , EffectiveAt(..)
       , fromUTCTime
       , toUTCTime
       , ResourceState(..)
       , Reference(..)
       , Void
       ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Char8 as B
import           Data.Default (def)
import qualified Data.Scientific as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Clock (DiffTime, UTCTime(..))
import           Data.Time.ISO8601 (parseISO8601, formatISO8601Millis)
import           Data.Typeable (Typeable)
import qualified Data.Void as V
import           GHC.Generics (Generic)
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types.Method as M


-- ResourceState
data ResourceState = Active | Archived deriving (Eq, Show)


-- EffectiveAt
newtype EffectiveAt = EffectiveAt { toUTCTime :: UTCTime }
                      deriving (Eq, Show)

fromUTCTime :: UTCTime -> EffectiveAt
fromUTCTime = EffectiveAt . roundUTCTimeToMillis

roundUTCTimeToMillis :: UTCTime -> UTCTime
roundUTCTimeToMillis (UTCTime day dayTime) =
  UTCTime day $ roundDiffToMillis dayTime
  where roundDiffToMillis = (/1000) . fromInt . truncate . (*1000)
        fromInt = fromIntegral :: Int -> DiffTime

instance A.FromJSON EffectiveAt where
  parseJSON = A.withText "ISO8601" (outer . parseISO8601 . T.unpack)
    where outer = maybe mempty (pure . EffectiveAt)

instance A.ToJSON EffectiveAt where
  toJSON = A.String . T.pack . formatISO8601Millis . toUTCTime


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


-- AccountingValue
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


data BalanceValue = BalanceValue DebitValue CreditValue AccountingValue
                  deriving (Eq, Show)

data DebitValue = DebitValue S.Scientific
                | DebitZeroValue
                deriving (Eq, Show)

data CreditValue = CreditValue S.Scientific
                 | CreditValueZero
                 deriving (Eq, Show)


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

  toBodyObject :: A.ToJSON a => q -> Maybe a
  toBodyObject = const Nothing

  toBody :: q -> HTTP.RequestBody
  toBody = maybe (HTTP.RequestBodyBS "") (HTTP.RequestBodyLBS . A.encode) . toBodyObject

  toRequest :: q -> HTTP.Request
  toRequest q = def { HTTP.method = toMethod q
                    , HTTP.path = toPath q
                    , HTTP.requestBody = toBody q
                    }


-- Void
newtype Void = Void V.Void deriving (Eq, Show)

instance A.ToJSON Void where
  toJSON = error "the void is uninhabited"
