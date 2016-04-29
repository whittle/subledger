module Data.API.Subledger.Types
       ( ResourceState(..)
       ) where

data ResourceState = Active | Archived deriving (Eq, Show)
