module Main where

import Control.Monad.Trans.Free (FreeF(..), FreeT(..))
import Network.Http.Client (Connection)
import Network.API.Subledger.Client (SubledgerConfig(..), SubledgerError(..))
import Network.API.Subledger.Client.HttpStreams (withConnection, callAPI)
import Network.API.Subledger.Test.AllSpecs (allSpecs)
import Network.API.Subledger.Test.Prelude (Subledger, SubledgerRequestF(..))

main :: IO ()
main = allSpecs runSubledger

runSubledger :: SubledgerConfig
             -> Subledger a
             -> IO (Either SubledgerError a)
runSubledger config subledger =
  withConnection $ \conn ->
    runSubledger' conn config subledger

runSubledger' :: Connection
              -> SubledgerConfig
              -> Subledger a
              -> IO (Either SubledgerError a)
runSubledger' conn config (FreeT m) = do
  f <- m
  case f of
   (Pure a) -> return (Right a)
   (Free (SubledgerRequestF req decode')) -> do
     r <- callAPI conn decode' config req
     case r of
      (Left e) ->  return (Left e)
      (Right next) -> runSubledger' conn config next
