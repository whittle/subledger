{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.API.Subledger.Client.HttpStreams
       ( subledger
       , subledgerConn
       , withConnection
       , SubledgerRequest(..)
       , SubledgerError(..)
       , SubledgerConfig(..)
         -- * low-level
       , callAPI
       ) where

-- import           Control.Monad.Trans.Resource (runResourceT)
-- import qualified Network.HTTP.Simple as H

import           Control.Exception (finally, SomeException, try)
import           Control.Monad (when)
import           Data.Aeson (encode, FromJSON, fromJSON, json', Result(..), Value)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Default (def)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Network.API.Subledger.Client (handleStream, Method(..), nullBody, SubledgerConfig(..), SubledgerError(..), SubledgerErrorType(..), SubledgerRequest(..), SubledgerReturn)
import qualified Network.Http.Client as C
import           OpenSSL (withOpenSSL)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec as Streams
import           System.IO.Streams.Attoparsec (ParseException(..))

-- | Create a request to Subledger’s API
subledger :: FromJSON (SubledgerReturn a)
          => SubledgerConfig
          -> SubledgerRequest a
          -> IO (Either SubledgerError (SubledgerReturn a))
subledger config request =
  withConnection $ \conn -> subledgerConn conn config request

-- | Create a request to Subledger’s API using a connection opened
-- with `withConnection`
subledgerConn :: FromJSON (SubledgerReturn a)
              => C.Connection
              -> SubledgerConfig
              -> SubledgerRequest a
              -> IO (Either SubledgerError (SubledgerReturn a))
subledgerConn = flip callAPI fromJSON

-- | Open a connection to the Subledger API server
withConnection :: (C.Connection -> IO (Either SubledgerError a))
               -> IO (Either SubledgerError a)
withConnection f =
  withOpenSSL $ do
    ctx <- C.baselineContextSSL
    result <- try (C.openConnectionSSL ctx "api.subledger.com" 443) :: IO (Either SomeException C.Connection)
    case result of
     Left msg -> return $ Left $ def { errorType = ConnectionFailure
                                     , errorMsg = T.pack $ show msg
                                     }
     Right conn -> f conn `finally` C.closeConnection conn

-- | Convert from subledger-core Method type to http-stream Method type
m2m :: Method -> C.Method
m2m GET = C.GET
m2m PATCH = C.PATCH
m2m POST = C.POST

------------------------------------------------------------------------------
-- | Create a request to `Subledger`'s API over an existing connection
--
-- see also: 'withConnection'
-- FIXME: all connection errors should be
-- turned into a `SubledgerError`. But that is not yet implemented.
--
-- NOTES: this a pretty low-level function. You probably want `subledger`
-- or `subledgerConn`. If you call this function directly, you are
-- responsible for ensuring the JSON conversion function supplied is
-- correct for `SubledgerRequest`. In the rest of the library this
-- property is enforced automatically by the type-system. But adding
-- that constraint here made implementing the `Subledger` testing monad
-- difficult.
callAPI :: C.Connection                    -- ^ an open connection to the server (`withConnection`)
        -> (Value -> Result b)             -- ^ function to convert JSON result to Haskell Value
        -> SubledgerConfig                 -- ^ SubledgerConfig
        -> SubledgerRequest a              -- ^ SubledgerRequest
        -> IO (Either SubledgerError b)
callAPI conn fromJSON' SubledgerConfig {..} sreq@SubledgerRequest{..} = do
  req <- C.buildRequest $ do
    C.http (m2m method) $ encodeUtf8 path
    C.setAuthorizationBasic (encodeUtf8 apiKey) (encodeUtf8 apiSecret)
    C.setAccept "application/json"
    when (not $ nullBody sreq) $ C.setContentType "application/json"
    C.setHeader "Connection" "Keep-Alive"
    C.setTransferEncoding
  when debug $ print req
  if nullBody sreq
    then C.sendRequest conn req C.emptyBody
    else do
     let lbs = encode body
     when debug $ L.putStrLn lbs
     i <- Streams.fromLazyByteString lbs
     C.sendRequest conn req $ C.inputStreamBody i
  C.receiveResponse conn $ \response inputStream ->
    do when debug $ print response
       let statusCode = C.getStatusCode response
       v <- try (Streams.parseFromStream json' inputStream)
       let r = case v of
                (Left (ParseException msg)) -> Error msg
                (Right a) -> Success a
       return $ handleStream fromJSON' statusCode r

debug :: Bool
debug = True
