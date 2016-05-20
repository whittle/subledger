{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.API.Subledger.Types
       ( Action(..)
       , ResourceState(..)
       , Void(..)
       ) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import           Data.Default (def)
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Void (Void)
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types.Method as M

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

data ResourceState = Active | Archived deriving (Eq, Show)

instance A.ToJSON Void where
  toJSON = error "never encode void"
