{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Data.API.Subledger.Request
       ( SubledgerRequest(..)
       , SubledgerReturn
       , mkRequest
       , Method(..)
       ) where

import           Data.Aeson (encode, Encoding, toEncoding, ToJSON)
import qualified Data.ByteString.Lazy as L
import           Data.String (IsString(..))
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
mkRequest :: (ToJSON b) => Method -> [Text] -> Maybe b -> SubledgerRequest a
mkRequest m ps b = SubledgerRequest { method = m
                                    , path = toPath ps
                                    , body = encode <$> b
                                    }
  where toPath = T.concat . map (T.cons '/') . ("v2":)

-- | Type-level function mapping from request type to response type
type family SubledgerReturn a :: *
