{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.API.Subledger (ApiKey(..), getOrg, OrgId(..))
import qualified Data.Text as T
import           System.Environment (getArgs, getEnv)

getApiKey :: IO ApiKey
getApiKey = mkApiKey <$> getEnv "API_KEY" <*> getEnv "API_SECRET"
  where mkApiKey key secret = ApiKey (T.pack key) (T.pack secret)

main :: IO ()
main = do
  key <- getApiKey
  args <- getArgs
  let orgId = OrgId . T.pack $ head args
  res <- getOrg key orgId
  putStrLn $ show res
