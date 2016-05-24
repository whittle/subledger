module Data.API.Subledger.Util
       ( snakeCase
       ) where

import           Data.Char (isUpper, toLower)
import           Data.List (intercalate)
import           Data.List.Split (dropInitBlank, keepDelimsL, split, whenElt)

snakeCase :: String -> String
snakeCase = intercalate "_"
            . map (\(x:xs) -> toLower x : xs)
            . split (dropInitBlank . keepDelimsL $ whenElt isUpper)
