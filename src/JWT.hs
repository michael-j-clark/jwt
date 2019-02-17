{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module JWT where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens
import Crypto.MAC.HMAC
import Crypto.Hash.Algorithms
import Crypto.Hash
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteArray
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Internal as IB
import qualified Data.ByteString.Base64 as B64
import Data.Hex
import Data.Int
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar
import GHC.Generics



secret = "<<secret-here>>"

data Header = Header
  { typ :: T.Text
  , alg :: T.Text
  } deriving (Show,Generic)

data Payload = Payload
  {
    iss :: T.Text
  , iat :: Int64
  , exp :: Int64
  } deriving (Show,Generic)

instance ToJSON Header
instance ToJSON Payload

encodeHeaderPayload :: ByteString -> ByteString -> ByteString
encodeHeaderPayload header payload =  B64.encode header  <> "." <> ( B64.encode) payload 

sigHash :: ByteString -> ByteString
sigHash x = B64.encode $ Data.ByteArray.convert $  (hmacGetDigest $ (hmac (secret::ByteString) (x::ByteString) ::HMAC SHA256))

assembleJWT :: ByteString -> ByteString
assembleJWT headerPayload = headerPayload <> "." <> (sigHash headerPayload)