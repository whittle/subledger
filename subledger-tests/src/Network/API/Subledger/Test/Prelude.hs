{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.API.Subledger.Test.Prelude
       ( SubledgerSpec
       , Subledger
       , SubledgerRequestF(..)
       , (>>=)
       , (>>)
       , return
       , void
       , fail
       , liftIO
       , fromString
       , module X
       , Either(..)
       , isRight
       , Maybe(..)
       ) where

import           Prelude ((.), ($), Functor(..), id, IO, String)
import qualified Prelude as X hiding ((>>=), (>>), return)
import qualified Control.Monad as M
import qualified Control.Monad.Trans as M
import           Control.Monad.Trans.Free (FreeT(..), liftF)
import           Data.Aeson (FromJSON, fromJSON, Result, Value)
import           Data.Either (Either(..), isRight)
import           Data.Maybe (Maybe(..))
import           Data.String (fromString)
import           Network.API.Subledger.Client
import           Test.Hspec
import           Test.Hspec.Core.Spec (SpecM)

type SubledgerSpec = (forall a. Subledger a -> IO (Either SubledgerError a)) -> Spec

type Subledger = FreeT SubledgerRequestF IO

data SubledgerRequestF ret =
  forall req. SubledgerRequestF { getSubledgerRequest :: SubledgerRequest req
                                , decode              :: Value -> Result ret
                                }

instance Functor SubledgerRequestF where
  fmap f (SubledgerRequestF req d) = SubledgerRequestF req (fmap f . d)

toSubledgerRequestF :: (FromJSON ret, SubledgerReturn req ~ ret)
                    => SubledgerRequest req
                    -> SubledgerRequestF ret
toSubledgerRequestF (SubledgerRequest m p b) =
  SubledgerRequestF (SubledgerRequest m p b) fromJSON

------------------------------------------------------------------------------
-- A class which lifts 'SubledgerRequest a' to the 'Subledger' monad and
-- leaves everything else alone.
class SubledgerLift a where
  type LiftedType a
  subledgerLift :: a -> (LiftedType a)

instance (FromJSON (SubledgerReturn req)) => SubledgerLift (SubledgerRequest req) where
  type LiftedType (SubledgerRequest req) = Subledger (SubledgerReturn req)
  subledgerLift req = liftF $ toSubledgerRequestF req

instance SubledgerLift (Subledger a) where
  type LiftedType (Subledger a) = Subledger a
  subledgerLift = id

instance SubledgerLift (IO a) where
  type LiftedType (IO a) = IO a
  subledgerLift = id

instance SubledgerLift (SpecM a r) where
  type LiftedType (SpecM a r) = SpecM a r
  subledgerLift = id

------------------------------------------------------------------------------
-- hack the do-syntax and related functions to automatically turn
-- SubledgerReq values into monadic functions.
--
-- This is useful in the test suite where we a running a bunch of
-- back-to-back subledger transactions with little business logic in
-- between.

(>>=) :: (SubledgerLift t, M.Monad m, LiftedType t ~ m a)
      => t -> (a -> m b) -> m b
m >>= f = (subledgerLift m) M.>>= f

(>>) :: (SubledgerLift t, M.Monad m, LiftedType t ~ m a)
     => t -> m b -> m b
m >> n = m >>= \_ -> n

void :: (FromJSON (SubledgerReturn a))
     => SubledgerRequest a -> Subledger ()
void req = M.void (subledgerLift req)

fail :: (M.Monad m) => String -> m a
fail = M.fail

return :: (M.Monad m) => a -> m a
return = M.return

liftIO :: IO a -> Subledger a
liftIO io = M.liftIO io
