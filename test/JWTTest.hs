{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module JWTTest where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens
import Crypto.MAC.HMAC
import Crypto.Hash.Algorithms
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.ByteString.Lazy
import Data.ByteArray
import Data.Int
--import Data.Foldable
--import Data.List
import Data.Maybe
--import Data.String.Conversions
import qualified Data.Text as T
import Data.Time.Calendar
import JWT
import Test.QuickCheck




testHeaders :: [Header]
testHeaders =
  [ Header "JWT" "HMAC"
  , Header "JWT" "SHA256"
  ]

-- These two instances make life easy:
--
-- Testable Bool
-- (Arbitrary a, Show a, Testable prop) => Testable (a -> prop)

prop_commutativeAdd :: Int -> Int -> Bool
prop_commutativeAdd x y = x + y == y + x

main :: IO()
main =  do
  
  quickCheck prop_commutativeAdd
  print $ secretHash $ toStrict $  Data.Aeson.encode $ fromJust $ testHeaders ^? element 1
  --print $ alg $ fromJust $ JWTTest.testHeaders ^? element 0 
 
  --print (hmacGetDigest $ (hmac (secret::ByteString) ("Somethin To Hash"::ByteString) ::HMAC SHA256))
