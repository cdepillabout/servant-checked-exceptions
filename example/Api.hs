{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Api where

import Data.Aeson
       (FromJSON(parseJSON), ToJSON(toJSON), Value, withText)
import Data.Aeson.Types (Parser)
-- import Data.Proxy (Proxy(Proxy))
import Data.String (IsString)
import Data.Text (unpack)
import Servant.API (Capture, JSON, Post, (:>), (:<|>))
-- import Servant.Client (Client, ClientM, client)
-- import Servant.Docs
--        (ToParam(toParam), ToSample(toSamples), docs, markdown)
-- import Servant.Docs.Internal
--        (DocQueryParam(DocQueryParam), ParamKind(Normal))
import Text.Read (readMaybe)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

import Servant.Checked.Exceptions (Throws)

---------
-- API --
---------

type Api = ApiStrictSearch :<|> ApiLaxSearch

type ApiStrictSearch =
  "strict-search" :>
  Capture "query" SearchQuery :>
  Throws BadSearchTermErr :>
  Throws IncorrectCapitalization :>
  Post '[JSON] SearchResponse

type ApiLaxSearch =
  "lax-search" :>
  Capture "query" SearchQuery :>
  Throws BadSearchTermErr :>
  Post '[JSON] SearchResponse

------------------------------
-- Parameters and Responses --
------------------------------

newtype SearchQuery = SearchQuery
  { unSearchQuery :: String
  } deriving ( Eq
             , FromHttpApiData
             , FromJSON
             , IsString
             , Ord
             , Read
             , Show
             , ToHttpApiData
             , ToJSON
             )

newtype SearchResponse = SearchResponse
  { unSearchResponse :: String
  } deriving ( Eq
             , FromHttpApiData
             , FromJSON
             , IsString
             , Ord
             , Read
             , Show
             , ToHttpApiData
             , ToJSON
             )

------------
-- Errors --
------------

data BadSearchTermErr = BadSearchTermErr deriving (Eq, Read, Show)

instance ToJSON BadSearchTermErr where
  toJSON :: BadSearchTermErr -> Value
  toJSON = toJSON . show

instance FromJSON BadSearchTermErr where
  parseJSON :: Value -> Parser BadSearchTermErr
  parseJSON = withText "BadSearchTermErr" $
    maybe (fail "could not parse as BadSearchTermErr") pure . readMaybe . unpack

data IncorrectCapitalization = IncorrectCapitalization deriving (Eq, Read, Show)

instance ToJSON IncorrectCapitalization where
  toJSON :: IncorrectCapitalization -> Value
  toJSON = toJSON . show

instance FromJSON IncorrectCapitalization where
  parseJSON :: Value -> Parser IncorrectCapitalization
  parseJSON = withText "IncorrectCapitalization" $
    maybe (fail "could not parse as IncorrectCapitalization") pure . readMaybe . unpack

----------
-- Port --
----------

port :: Int
port = 8201
