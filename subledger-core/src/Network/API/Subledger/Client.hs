module Network.API.Subledger.Client
       ( SubledgerConfig(..)
       , module Data.API.Subledger.Request
       , module Data.API.Subledger.Error
       , handleStream
       , parseFail
       , unknownCode
       ) where

import           Data.Aeson (fromJSON, Result(..), Value)
import           Data.API.Subledger.Error
import           Data.API.Subledger.Request
import           Data.Default (def)
import           Data.Text (Text)
import qualified Data.Text as T

data SubledgerConfig =
  SubledgerConfig { apiKey :: Text
                  , apiSecret :: Text
                  } deriving (Eq, Show)

-- | Used by backends such as @subledger-http-client@ to decode the
-- results of an API request.
handleStream :: (Value -> Result a) -- ^ function to decode JSON value
             -> Int                 -- ^ HTTP response code
             -> Result Value        -- ^ result of attempting to decode body
             -> Either SubledgerError a
handleStream decodeValue statusCode r =
  case r of
   Error message -> parseFail message statusCode
   (Success value) ->
     case statusCode of
      code | code < 300 ->
               case decodeValue value of
                Error message -> parseFail message statusCode
                Success a -> Right a
      code | code >= 400 ->
               case fromJSON value of
                Error message -> parseFail message statusCode
                Success subledgerError -> Left $ setErrorHTTP code subledgerError
      _ -> unknownCode statusCode
  -- case statusCode of
  --  code | code < 300 -> case r of
  --          Error message -> parseFail message
  --          (Success value) ->
  --            case decodeValue value of
  --             (Error message) -> parseFail statusCode message
  --             (Success a) -> (Right a)
  --  code | code >= 400 ->
  --           case r of
  --            Error message -> parseFail message
  --            (Success value) ->
  --              case fromJSON value of
  --               (Error message) -> parseFail message
  --               (Success subledgerError) ->
  --                 Left $ setErrorHTTP code subledgerError
  --  _ -> unknownCode

-- | lift a parser error to be a SubledgerError
parseFail :: String  -- ^ error message
          -> Int -- ^ HTTP status code
          -> Either SubledgerError a
parseFail errorMessage code =
  Left $ def { errorType = ParseFailure
             , errorMsg = T.pack errorMessage
             , errorHTTP = Just $ mkErrorHTTP code
             }

-- | `SubledgerError` to return when we don't know what to do with the
-- received HTTP status code.
unknownCode :: Int -> Either SubledgerError a
unknownCode code =
  Left $ def { errorType = UnknownErrorType
             , errorHTTP = Just $ mkErrorHTTP code
             }
