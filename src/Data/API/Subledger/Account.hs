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

import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Aeson.Types (Options(..))
import qualified Data.Text as T
import           GHC.Generics (Generic)
import           Network.HTTP.Types.Method (methodPatch, methodPost)

import Data.API.Subledger.Book (BookId(..))
import Data.API.Subledger.Org (OrgId(..))
import Data.API.Subledger.Types
import Data.API.Subledger.Util

newtype AccountId = AccountId { unAccountId :: T.Text }
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

data AccountBody = AccountBody { accountBodyDescription :: T.Text
                               , accountBodyReference :: Maybe T.Text
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
