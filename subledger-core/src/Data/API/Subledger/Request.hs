{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.API.Subledger.Request
       ( SubledgerRequest(..)
       , SubledgerReturn
       , mkRequest
       , mkEmptyRequest
       , nullBody
       , Method(..)
       ) where

import           Data.Aeson (encode, ToJSON(..))
import           Data.Aeson.Types ((.=), Object, object, Pair, pairs, Series)
import qualified Data.ByteString.Lazy as L
import           Data.Default (Default(..))
import           Data.HashMap.Strict (empty, foldrWithKey, insert)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)

-- | HTTP method
--
-- The other methods are not required by the Subledger API.
data Method = GET | PATCH | POST
            deriving (Eq, Ord, Read, Show, Typeable)

-- | Types that can be translated into URL path pieces.
class PathPiece a where
  toUrlText :: a -> Text

instance PathPiece Text where
  toUrlText = id

newtype BodyObject = BodyObject { unBody :: Object }
                   deriving (Eq, Show, Typeable)

instance ToJSON BodyObject where
  toEncoding (BodyObject o) = pairs $ toSeries o
  toJSON (BodyObject o) = object $ toPairs o

-- | HTTP request to be submitted to Subledger. Intended to be
-- Subledger-specific, but directly translatable to an HTTP request
-- specific to the HTTP client.
data SubledgerRequest a =
  SubledgerRequest { method :: Method
                   , path :: Text
                   , query :: [(Text, Text)]
                   , body :: BodyObject
                   } deriving (Eq, Show, Typeable)

instance Default (SubledgerRequest a) where
  def = SubledgerRequest GET mempty mempty $ BodyObject mempty

nullBody :: SubledgerRequest a -> Bool
nullBody (SubledgerRequest _ _ _ body) = null $ unBody body

-- | Helper method for creating `SubledgerRequest`s
mkRequest :: Method -> [Text] -> [Pair] -> SubledgerRequest a
mkRequest m ps b = def { method = m
                       , path = toPath ps
                       , body = BodyObject $ toObject b
                       }

toObject :: [Pair] -> Object
toObject = foldr addPairToObject empty

addPairToObject :: Pair -> Object -> Object
addPairToObject (k, v) = insert k v

toPairs :: Object -> [Pair]
toPairs = foldrWithKey f []
  where f k v = ((k, v) :)

toSeries :: Object -> Series
toSeries = foldrWithKey f mempty
  where f k v = mappend $ k .= v

mkEmptyRequest :: [Text] -> SubledgerRequest a
mkEmptyRequest ps = def { path = toPath ps
                        }

toPath :: [Text] -> Text
toPath = T.intercalate "/" . ("":) . ("v2":)

-- class HasParam request param where

-- | Type-level function mapping from request type to response type
type family SubledgerReturn a :: *
