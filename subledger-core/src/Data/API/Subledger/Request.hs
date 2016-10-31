{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.API.Subledger.Request
       ( SubledgerRequest(..)
       , HasParam(..)
       , SubledgerReturn
       , mkRequest
       , mkEmptyRequest
       , nullBody
       , addPairToRequestBody
       , adjustBodyObject
       , (-&-)
       , Method(..)
       ) where

import           Data.Aeson.Types ((.=), Object, object, Pair, pairs, Series, ToJSON(..))
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
nullBody (SubledgerRequest _ _ _ b) = null $ unBody b

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

addPairToRequestBody :: Pair -> SubledgerRequest a -> SubledgerRequest a
addPairToRequestBody p r@(SubledgerRequest _ _ _ (BodyObject o)) =
  r { body = BodyObject $ addPairToObject p o }

toPairs :: Object -> [Pair]
toPairs = foldrWithKey f []
  where f k v = ((k, v) :)

toSeries :: Object -> Series
toSeries = foldrWithKey f mempty
  where f k v = mappend $ k .= v

mkEmptyRequest :: Method -> [Text] -> SubledgerRequest a
mkEmptyRequest m ps = def { method = m
                          , path = toPath ps
                          }

adjustBodyObject :: (Object -> Object) -> SubledgerRequest a -> SubledgerRequest a
adjustBodyObject f r@(SubledgerRequest _ _ _ (BodyObject o)) =
  r { body = BodyObject $ f o }

toPath :: [Text] -> Text
toPath = T.intercalate "/" . ("":) . ("v2":)


-- | Class used to indicate a relation between a request type and a
-- param it can optionally accept.
class HasParam action param where
  addParam :: param -> SubledgerRequest action -> SubledgerRequest action


-- | Infix operator for adding an optional param to a request.
(-&-) :: HasParam action param
      => SubledgerRequest action
      -> param
      -> SubledgerRequest action
(-&-) = flip addParam


-- | Type-level function mapping from request type to response type
type family SubledgerReturn a :: *
