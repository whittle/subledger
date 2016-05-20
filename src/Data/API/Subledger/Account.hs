{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Data.API.Subledger.Account
       ( AccountId(..)
       , AccountNormalBalance(..)
       , AccountBody(..)
       , Account(..)
       ) where

import           Control.Applicative ((<|>), empty)
import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Network.HTTP.Types.Method (methodPatch, methodPost)

import Data.API.Subledger.Book (BookId(..))
import Data.API.Subledger.Org (OrgId(..))
import Data.API.Subledger.Types

newtype AccountId = AccountId { unAccountId :: T.Text }
                  deriving (Eq, Show)

data AccountNormalBalance = Credit | Debit
                          deriving (Eq, Show)

instance ToJSON AccountNormalBalance where
  toJSON Credit = String "credit"
  toJSON Debit = String "debit"

instance FromJSON AccountNormalBalance where
  parseJSON (String "credit") = pure Credit
  parseJSON (String "debit") = pure Debit
  parseJSON _ = empty

data AccountBody = AccountBody { accountDescription :: T.Text
                               , accountReference :: Maybe T.Text
                               , accountNormalBalance :: AccountNormalBalance
                               } deriving (Eq, Generic, Show)

instance ToJSON AccountBody -- provided by Generic

data Account = Account { accountId :: AccountId
                       , accountState :: ResourceState
                       , accountVersion :: Int
                       , accountBody :: AccountBody
                       , accountBookId :: Maybe BookId
                       } deriving (Eq, Show)

mkAccount :: ResourceState
             -> T.Text
             -> Int
             -> T.Text
             -> Maybe T.Text
             -> AccountNormalBalance
             -> Maybe T.Text
             -> Account
mkAccount s i v d r n b = Account { accountId = AccountId i
                                  , accountState = s
                                  , accountVersion = v
                                  , accountBody = AccountBody { accountDescription = d
                                                              , accountReference = r
                                                              , accountNormalBalance = n
                                                              }
                                  , accountBookId = BookId <$> b
                                  }

instance FromJSON Account where
  parseJSON (Object v) = ((Active,) <$> (v .: "active_account"))
                         <|> ((Archived,) <$> (v .: "archived_account"))
                         >>= inner
    where inner (s, Object v') = mkAccount s
                                 <$> v' .: "id"
                                 <*> v' .: "version"
                                 <*> v' .: "description"
                                 <*> v' .:? "reference"
                                 <*> v' .: "normal_balance"
                                 <*> v' .:? "book"
          inner _ = empty
  parseJSON _ = empty

data CreateAccount = CreateAccount OrgId BookId AccountBody deriving (Eq, Show)
instance Action CreateAccount AccountBody Account where
  toMethod = const methodPost
  toPathPieces (CreateAccount oid bid _) = ["orgs", unOrgId oid, "books", unBookId bid, "accounts"]
  toBodyObject (CreateAccount _ _ body) = Just body

data FetchAccounts = FetchAccounts OrgId BookId deriving (Eq, Show)
instance Action FetchAccounts Void [Account] where
  toPathPieces (FetchAccounts oid bid) = ["orgs", unOrgId oid, "books", unBookId bid, "accounts"]

data FetchAccount = FetchAccount OrgId BookId AccountId deriving (Eq, Show)
instance Action FetchAccount Void Account where
  toPathPieces (FetchAccount oid bid aid) = [ "orgs"
                                            , unOrgId oid
                                            , "books"
                                            , unBookId bid
                                            , "accounts"
                                            , unAccountId aid
                                            ]

data PatchAccount = PatchAccount OrgId BookId Account deriving (Eq, Show)
instance Action PatchAccount AccountBody Account where
  toMethod = const methodPatch
  toPathPieces (PatchAccount oid bid account) = [ "orgs"
                                                , unOrgId oid
                                                , "books"
                                                , unBookId bid
                                                , "accounts"
                                                , unAccountId $ accountId account
                                                ]
  toBodyObject (PatchAccount _ _ account) = Just $ accountBody account

-- GET /orgs/{org_id}/books/{book_id}/accounts/{account_id}/lines
-- POST /orgs/{org_id}/books/{book_id}/accounts/{account_id}/archive
-- POST /orgs/{org_id}/books/{book_id}/accounts/{account_id}/activate
-- GET /orgs/{org_id}/books/{book_id}/accounts/{account_id}/balance
-- GET /orgs/{org_id}/books/{book_id}/accounts/{account_id}/first_and_last_line
