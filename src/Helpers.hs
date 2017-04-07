

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Helpers where

import Text.XML.HXT.Core

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)

-- TODO import qualified
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BLC

import Data.Char(isSpace)

import Database.PostgreSQL.Simple as PG(query, execute, connectPostgreSQL)

-- http://stackoverflow.com/questions/34547937/haskell-import-qualified-and-not-in-scope-data-constructor
import Database.PostgreSQL.Simple.Types as PG(Only(..))


import Text.RawString.QQ

-- Helpers

parseXML s = readString [ withValidate no
    , withRemoveWS yes  -- throw away formating WS
    ] s


atTag tag = deep (isElem >>> hasName tag)

atChildName s = getChildren >>> hasName s

getChildText = getChildren >>> getText

stripSpace = filter $ not.isSpace


