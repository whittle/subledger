{-# LANGUAGE RankNTypes #-}

module Network.API.Subledger.Test.Config
       ( getConfig
       ) where

import Data.Maybe (fromJust)
import Data.Text (pack)
import Network.API.Subledger.Client (SubledgerConfig(..), SubledgerError)
import Network.API.Subledger.Test.Prelude (Subledger)
import System.Environment (lookupEnv)

getConfig :: (forall a. SubledgerConfig -> Subledger a -> IO (Either SubledgerError a))
          -> IO SubledgerConfig
getConfig _ = SubledgerConfig <$> (fmap (pack . fromJust) $ lookupEnv "SUBLEDGER_TEST_KEY")
                              <*> (fmap (pack . fromJust) $ lookupEnv "SUBLEDGER_TEST_SECRET")
