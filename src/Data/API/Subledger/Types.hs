{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.API.Subledger.Types
       ( AccountingValue(..)
       , Action(..)
       , BalanceValue(..)
       , CreditValue(..)
       , DebitValue(..)
       , EffectiveAt
       , fromUTCTime
       , toUTCTime
       , ResourceState(..)
       , Void
       ) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import           Data.Default (def)
import qualified Data.Scientific as S
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Clock (DiffTime, UTCTime(..))
import           Data.Time.ISO8601 (parseISO8601, formatISO8601Millis)
import qualified Data.Void as V
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types.Method as M

data ResourceState = Active | Archived deriving (Eq, Show)

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
  parseJSON (A.String s) = outer . parseISO8601 $ T.unpack s
    where outer (Just t) = pure $ EffectiveAt t
          outer _ = mempty
  parseJSON _ = mempty

instance A.ToJSON EffectiveAt where
  toJSON = A.String . T.pack . formatISO8601Millis . toUTCTime

data AccountingValue = AccountingDebitValue S.Scientific
                     | AccountingCreditValue S.Scientific
                     | AccountingZeroValue
                     deriving (Eq, Show)

data BalanceValue = BalanceValue DebitValue CreditValue AccountingValue
                  deriving (Eq, Show)

data DebitValue = DebitValue S.Scientific
                | DebitZeroValue
                deriving (Eq, Show)

data CreditValue = CreditValue S.Scientific
                 | CreditValueZero
                 deriving (Eq, Show)

class A.ToJSON a => Action q a r | q -> a r where
  toMethod :: q -> M.Method
  toMethod = const M.methodGet

  toPathPieces :: q -> [T.Text]

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

newtype Void = Void V.Void deriving (Eq, Show)

instance A.ToJSON Void where
  toJSON = error "the void is uninhabited"
