{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Data.API.Subledger.Account
       ( AccountId(..)
       , AccountNormalBalance(..)
       , AccountBody(..)
       , Account(..)
       , createAccount
       , fetchAccount
       ) where

import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Aeson.Types (Options(..))
import           Data.API.Subledger.Book (BookId(..))
import           Data.API.Subledger.Org (OrgId(..))
import           Data.API.Subledger.Request
import           Data.API.Subledger.Types
import           Data.API.Subledger.Util
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Network.HTTP.Types.Method (methodPatch)

newtype AccountId = AccountId { unAccountId :: Text }
                  deriving (Eq, Show)

instance FromJSON AccountId where
  parseJSON (String s) = pure $ AccountId s
  parseJSON _ = mempty

data AccountNormalBalance = CreditNormal | DebitNormal
                          deriving (Eq, Show)

instance ToJSON AccountNormalBalance where
  toJSON CreditNormal = String "credit"
  toJSON DebitNormal = String "debit"

instance FromJSON AccountNormalBalance where
  parseJSON (String "credit") = pure CreditNormal
  parseJSON (String "debit") = pure DebitNormal
  parseJSON _ = mempty

data AccountBody = AccountBody { accountBodyDescription :: Text
                               , accountBodyReference :: Maybe Text
                               , accountBodyNormalBalance :: AccountNormalBalance
                               } deriving (Eq, Generic, Show)

accountBodyFields :: String -> String
accountBodyFields = snakeCase . drop 11

instance FromJSON AccountBody where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = accountBodyFields }

instance ToJSON AccountBody where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = accountBodyFields }

data Account = Account { accountId :: AccountId
                       , accountState :: ResourceState
                       , accountVersion :: Int
                       , accountBody :: AccountBody
                       , accountBookId :: Maybe BookId
                       } deriving (Eq, Show)

instance FromJSON Account where
  parseJSON (Object v) = ((Active,) <$> (v .: "active_account"))
                         <|> ((Archived,) <$> (v .: "archived_account"))
                         >>= inner
    where inner (s, Object v') = Account <$> v' .: "id"
                                         <*> pure s
                                         <*> v' .: "version"
                                         <*> parseJSON (Object v')
                                         <*> v' .:? "book"
          inner _ = mempty
  parseJSON _ = mempty

data CreateAccount
type instance SubledgerReturn CreateAccount = Account

createAccount :: OrgId
              -> BookId
              -> Text -- ^ description
              -> AccountNormalBalance
              -> SubledgerRequest CreateAccount
createAccount (OrgId oid) (BookId bid) s b =
  mkRequest POST
            ["orgs", oid, "books", bid, "accounts"]
            [("description", String s), ("normal_balance", toJSON b)]

data FetchAccounts = FetchAccounts OrgId BookId deriving (Eq, Show)
instance Action FetchAccounts Void [Account] where
  toPathPieces (FetchAccounts oid bid) = ["orgs", unOrgId oid, "books", unBookId bid, "accounts"]

data FetchAccount
type instance SubledgerReturn FetchAccount = Account

fetchAccount :: OrgId -> BookId -> AccountId -> SubledgerRequest FetchAccount
fetchAccount (OrgId oid) (BookId bid) (AccountId aid) =
  mkEmptyRequest ["orgs", oid, "books", bid, "accounts", aid]

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
