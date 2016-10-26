{-# LANGUAGE DeriveDataTypeable #-}

module Data.API.Subledger.Error
       ( SubledgerError(..)
       , SubledgerErrorType(..)
       , SubledgerErrorHTTPCode(..)
       , mkErrorHTTP
       , setErrorHTTP
       ) where

import Control.Exception
import Data.Aeson
import Data.Default
import Data.Text (Text)
import Data.Typeable (Typeable)

-- | Error Codes for HTTP Responses
data SubledgerErrorHTTPCode = BadRequest               -- ^ 400
                            | UnAuthorized             -- ^ 401
                            | RequestFailed            -- ^ 402
                            | NotFound                 -- ^ 404
                            | VersionConflict          -- ^ 409
                            | SubledgerServerError Int -- ^ (>=500)
                            | UnknownHTTPCode Int      -- ^ All other codes
                         deriving (Eq, Show, Typeable)

mkErrorHTTP :: Int -> SubledgerErrorHTTPCode
mkErrorHTTP statusCode =
  case statusCode of
   400 -> BadRequest
   401 -> UnAuthorized
   402 -> RequestFailed
   404 -> NotFound
   409 -> VersionConflict
   code | code >= 500 -> SubledgerServerError code
   code -> UnknownHTTPCode code

-- | set the `errorHTTP` field of the `SubledgerError` based on the
-- HTTP response code.
setErrorHTTP :: Int            -- ^ HTTP Status code
             -> SubledgerError -- ^ `SubledgerError`
             -> SubledgerError
setErrorHTTP statusCode subledgerError = subledgerError { errorHTTP = Just $ mkErrorHTTP statusCode }

-- | Subledger error types
data SubledgerErrorType = ConnectionFailure
                        | ParseFailure
                        | APIError
                        | UnknownErrorType
                        | UnlabeledErrorType
                        deriving (Eq, Show, Typeable)

-- | Subledger error
data SubledgerError =
  SubledgerError { errorType :: SubledgerErrorType
                 , errorMsg :: Text
                 , errorHTTP :: Maybe SubledgerErrorHTTPCode
                 } deriving (Eq, Show, Typeable)

instance Default SubledgerError where
  def = SubledgerError UnlabeledErrorType mempty Nothing

instance Exception SubledgerError

instance FromJSON SubledgerError where
  parseJSON = undefined --withObject "SubledgerError" $ \o ->
              -- SubledgerError <$>
