{-# LANGUAGE RankNTypes #-}

module Network.API.Subledger.Test.AllSpecs (allSpecs) where

import           Test.Hspec (hspec)
import           Network.API.Subledger.Client (SubledgerConfig, SubledgerError)
import           Network.API.Subledger.Test.Config (getConfig)
import           Network.API.Subledger.Test.Prelude (Subledger)
import qualified Network.API.Subledger.Test.Account
import qualified Network.API.Subledger.Test.Book
import qualified Network.API.Subledger.Test.Org

-- | Main test function entry point
allSpecs :: (forall a. SubledgerConfig -> Subledger a -> IO (Either SubledgerError a))
         -> IO ()
allSpecs subledger' = do
  config <- getConfig subledger'
  let subledger = subledger' config
  hspec $ do
    Network.API.Subledger.Test.Org.spec subledger
    Network.API.Subledger.Test.Book.spec subledger
    Network.API.Subledger.Test.Account.spec subledger
