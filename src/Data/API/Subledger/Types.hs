{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.API.Subledger.Types
       ( Action(..)
       , EffectiveAt(..)
       , ResourceState(..)
       , Void
       ) where

import           Control.Applicative (empty)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import           Data.Default (def)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.ISO8601 (parseISO8601)
import qualified Data.Void as V
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types.Method as M

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

data ResourceState = Active | Archived deriving (Eq, Show)

newtype EffectiveAt = EffectiveAt { unEffectiveAt :: UTCTime }
                      deriving (Eq, Show)

instance A.FromJSON EffectiveAt where
  parseJSON (A.String s) = outer . parseISO8601 $ T.unpack s
    where outer (Just t) = pure $ EffectiveAt t
          outer _ = empty
  parseJSON _ = empty

newtype Void = Void V.Void deriving (Eq, Show)

instance A.ToJSON Void where
  toJSON = error "the void is uninhabited"
