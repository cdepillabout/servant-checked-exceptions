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

module Servant.Checked.Exceptions where

-- Imports for Servant Stuff
import Data.Aeson
       (FromJSON(parseJSON), ToJSON(toJSON), Value, withText)
import Data.Aeson.Types (Parser)
import Data.Proxy (Proxy(Proxy))
import Data.String (IsString)
import Data.Text (Text, unpack)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
       (Handler, HasServer(..), JSON, Post, QueryParam, Server, ServerT,
        (:>), enter, serve)
import Servant.Client (Client, ClientM, client)
import Servant.Docs
       (ToParam(toParam), ToSample(toSamples), docs, markdown)
import Servant.Docs.Internal
       (DocQueryParam(DocQueryParam), ParamKind(Normal))
import Text.Read (readMaybe)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)

-- This changes in servant-0.10
-- import Control.Natural ((:~>)(NT))
import Servant.Utils.Enter ((:~>)(Nat))

import Servant.Checked.Exceptions.Internal (Envelope, Throwing, Throws, pureErrEnvelope, pureSuccEnvelope)

---------
-- API --
---------

type Api = ApiSearch

type ApiSearch =
  "search" :>
  QueryParam "query" SearchQuery :>
  Throws FooErr :>
  Throws BarErr :>
  Post '[JSON] SearchResponse

newtype SearchQuery =
  SearchQuery String
  deriving ( Eq
           , FromHttpApiData
           , FromJSON
           , IsString
           , Ord
           , Read
           , Show
           , ToHttpApiData
           , ToJSON
           )

newtype SearchResponse =
  SearchResponse String
  deriving ( Eq
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
-- Server --
------------

serverRoot :: ServerT Api Handler
serverRoot = postSearchServer

postSearchServer
  :: Maybe SearchQuery
  -> Handler (Envelope '[FooErr, BarErr] SearchResponse)
postSearchServer maybeQ =
  case maybeQ of
    Just "hello" -> pureErrEnvelope BarErr
    Just "Hello" -> pureSuccEnvelope "good"
    _ -> pureErrEnvelope FooErr

-- | Given a 'Config', this returns a Wai 'Application'.
app :: Application
app = serve (Proxy :: Proxy Api) apiServer

-- | Given a 'Config', this returns a servant 'Server' for 'Api'
apiServer :: Server Api
apiServer = enter natTrans serverRoot
  where
    natTrans :: Handler :~> Handler
    natTrans = Nat trans

    trans :: forall a. Handler a -> Handler a
    trans = id

defaultMainServer :: IO ()
defaultMainServer = run 8201 app

------------
-- Client --
------------

postSearchClient
  :: Maybe SearchQuery
  -> ClientM (Envelope '[FooErr, BarErr] SearchResponse)
postSearchClient = client (Proxy :: Proxy Api)

----------
-- Docs --
----------

instance ToSample SearchResponse where
  toSamples :: Proxy SearchResponse -> [(Text, SearchResponse)]
  toSamples Proxy = [("This is a successful response.", "good")]

instance ToParam (QueryParam "query" SearchQuery) where
  toParam :: Proxy (QueryParam "query" SearchQuery) -> DocQueryParam
  toParam Proxy = DocQueryParam "query" [] "The search query." Normal

instance ToSample FooErr where
  toSamples :: Proxy FooErr -> [(Text, FooErr)]
  toSamples Proxy = [("this is a fooerr", FooErr)]

instance ToSample BarErr where
  toSamples :: Proxy BarErr -> [(Text, BarErr)]
  toSamples Proxy = [("this is a barerr", BarErr)]

instance ToSample BazErr where
  toSamples :: Proxy BazErr -> [(Text, BazErr)]
  toSamples Proxy = [("this is a BazErr", BazErr)]

createDocs :: IO ()
createDocs = putStrLn . markdown $ docs (Proxy :: Proxy Api)

------------
-- Errors --
------------

data FooErr = FooErr deriving (Eq, Read, Show)

instance ToJSON FooErr where
  toJSON :: FooErr -> Value
  toJSON = toJSON . show

instance FromJSON FooErr where
  parseJSON :: Value -> Parser FooErr
  parseJSON =
    withText "FooErr" $
      maybe (fail "could not parse as FooErr") pure . readMaybe . unpack

data BarErr = BarErr deriving (Eq, Read, Show)

instance ToJSON BarErr where
  toJSON :: BarErr -> Value
  toJSON = toJSON . show

instance FromJSON BarErr where
  parseJSON :: Value -> Parser BarErr
  parseJSON =
    withText "BarErr" $
      maybe (fail "could not parse as BarErr") pure . readMaybe . unpack

data BazErr = BazErr deriving (Eq, Read, Show)

instance ToJSON BazErr where
  toJSON :: BazErr -> Value
  toJSON = toJSON . show

instance FromJSON BazErr where
  parseJSON :: Value -> Parser BazErr
  parseJSON =
    withText "BazErr" $
      maybe (fail "could not parse as BazErr") pure . readMaybe . unpack
