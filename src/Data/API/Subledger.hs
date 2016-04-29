{-# LANGUAGE OverloadedStrings #-}

module Data.API.Subledger
       ( ApiKey(..)
       , getOrg
       , OrgId(..)
       , Org(..)
       ) where

import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Simple as H

import Data.API.Subledger.Org

data ApiKey = ApiKey { apiKeyId :: T.Text
                     , apiKeySecret :: T.Text
                     }

apiEndpointUrl :: T.Text -> T.Text
apiEndpointUrl oId = T.intercalate "/" ["https://api.subledger.com", "v2", "orgs", oId]

getOrg :: ApiKey -> OrgId -> IO (H.Response Org)
getOrg apiKey oId = runResourceT $ do
  initReq <- H.parseRequest . T.unpack . apiEndpointUrl $ unOrgId oId
  let req = H.setRequestBasicAuth (encodeUtf8 $ apiKeyId apiKey) (encodeUtf8 $ apiKeySecret apiKey)
          $ H.setRequestSecure True
          $ initReq
  H.httpJSON req
