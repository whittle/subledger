{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.API.Subledger.Request
       ( SubledgerRequest(..)
       , SubledgerReturn
       , mkRequest
       , mkEmptyRequest
       , Method(..)
       ) where

import           Data.Aeson (encode, ToJSON)
import qualified Data.ByteString.Lazy as L
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

-- | Stripe Request holding `Method`, URL and body for a Request.
data SubledgerRequest a =
  SubledgerRequest { method :: Method
                   , path :: Text
                   , body :: Maybe L.ByteString
                   } deriving (Eq, Show, Typeable)

-- | Helper method for creating `SubledgerRequest`s
mkRequest :: (ToJSON b) => Method -> [Text] -> b -> SubledgerRequest a
mkRequest m ps b = SubledgerRequest { method = m
                                    , path = toPath ps
                                    , body = Just $ encode b
                                    }

mkEmptyRequest :: Method -> [Text] -> SubledgerRequest a
mkEmptyRequest m ps = SubledgerRequest { method = m
                                       , path = toPath ps
                                       , body = Nothing
                                       }

toPath :: [Text] -> Text
toPath = T.intercalate "/" . ("":) . ("v2":)

-- | Type-level function mapping from request type to response type
type family SubledgerReturn a :: *
