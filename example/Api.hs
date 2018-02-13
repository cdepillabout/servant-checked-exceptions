{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Aeson
       (FromJSON(parseJSON), ToJSON(toJSON), Value, withText)
import Data.Aeson.Types (Parser)
import Data.String (IsString)
import Data.Text (unpack)
import Network.HTTP.Types (Status, status400, status404)
import Servant.API (Capture, JSON, Post, (:>), (:<|>))
import Text.Read (readMaybe)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

import Servant.Checked.Exceptions (NoThrow, Throws)
import Servant.Checked.Exceptions.Internal.Servant.API (ErrStatus(toErrStatus), GetWithEx)

---------
-- API --
---------

-- | This is our main 'Api' type.  We will create a server, a client, and
-- documentation for this api.
--
-- This api is composed of three routes, 'ApiStrictSearch', 'ApiLaxSearch', and
-- 'ApiNoErrSearch'.
type Api = ApiStrictSearch :<|> ApiLaxSearch :<|> ApiNoErrSearch :<|> ApiTestTest

-- | This is a strict search api.  You pass it a @\"query\"@, and it returns a
-- 'SearchResponse'.  It potentially returns a 'BadSearchTermErr' if your query
-- is not the string @\"hello\"@.  It returns an 'IncorrectCapitialization'
-- error if your query is not capitalized like @\"Hello\"@.
--
-- Notice how we are using 'Throws' to indicate we will potentially throw an
-- error.  Also, notice how we can list multiple 'Throws'.
type ApiStrictSearch =
  "strict-search" :>
  Capture "query" SearchQuery :>
  Throws BadSearchTermErr :>
  Throws IncorrectCapitalization :>
  Post '[JSON] SearchResponse

-- | This is similar to 'ApiStrictSearch', but it doesn't force the query to be
-- capitalized correctly.  It only returns a 'BadSearchTermErr'.
type ApiLaxSearch =
  "lax-search" :>
  Capture "query" SearchQuery :>
  Throws BadSearchTermErr :>
  Post '[JSON] SearchResponse

-- | This is similar to 'ApiLaxSearch', but it doesn't force the query to use
-- correct terms.  It does not return an error.
type ApiNoErrSearch =
  "no-err-search" :>
  Capture "query" SearchQuery :>
  NoThrow :>
  Post '[JSON] SearchResponse

type ApiTestTest =
  "testtesttest" :>
  Capture "query" SearchQuery :>
  GetWithEx '[JSON] '[BadSearchTermErr, IncorrectCapitalization] SearchResponse

------------------------------
-- Parameters and Responses --
------------------------------

-- | This 'SearchQuery' type is just a newtype wrapper around a 'String'.
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

-- | This 'SearchResponse' type is just a newtype wrapper around a 'String'.
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

-- | This error is returned when the search query is not the string @\"hello\"@.
data BadSearchTermErr = BadSearchTermErr deriving (Eq, Read, Show)

instance ToJSON BadSearchTermErr where
  toJSON :: BadSearchTermErr -> Value
  toJSON = toJSON . show

instance FromJSON BadSearchTermErr where
  parseJSON :: Value -> Parser BadSearchTermErr
  parseJSON = withText "BadSearchTermErr" $
    maybe (fail "could not parse as BadSearchTermErr") pure . readMaybe . unpack

instance ErrStatus BadSearchTermErr where
  toErrStatus :: BadSearchTermErr -> Status
  toErrStatus _ = status404

-- | This error is returned when the search query is @\"hello\"@, but it is not
-- capitalized correctly.  For example, the search query @\"hello\"@ will
-- return an 'IncorrectCapitialization' error.  However, the search query
-- @\"Hello\"@ will return a success.
data IncorrectCapitalization = IncorrectCapitalization deriving (Eq, Read, Show)

instance ToJSON IncorrectCapitalization where
  toJSON :: IncorrectCapitalization -> Value
  toJSON = toJSON . show

instance FromJSON IncorrectCapitalization where
  parseJSON :: Value -> Parser IncorrectCapitalization
  parseJSON = withText "IncorrectCapitalization" $
    maybe (fail "could not parse as IncorrectCapitalization") pure . readMaybe . unpack

instance ErrStatus IncorrectCapitalization where
  toErrStatus :: IncorrectCapitalization -> Status
  toErrStatus _ = status400

----------
-- Port --
----------

-- | The port to run the server on.
port :: Int
port = 8201
