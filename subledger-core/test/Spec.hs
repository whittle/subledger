module Main where

import qualified Data.API.Subledger.AccountTest
import qualified Data.API.Subledger.BookTest
import qualified Data.API.Subledger.JournalEntryTest
import qualified Data.API.Subledger.OrgTest
import qualified Data.API.Subledger.TypesTest
import           Test.Tasty (defaultMain, testGroup, TestTree)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests"
        [ Data.API.Subledger.AccountTest.suite
        , Data.API.Subledger.BookTest.suite
        , Data.API.Subledger.JournalEntryTest.suite
        , Data.API.Subledger.OrgTest.suite
        , Data.API.Subledger.TypesTest.suite
        ]
