{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
import Data.Text (unpack)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
       (Handler, HasServer(..), JSON, Post, QueryParam, Server, ServerT,
        (:>), enter, serve)
import Text.Read (readMaybe)

-- This changes in servant-0.10
-- import Control.Natural ((:~>)(NT))
import Servant.Utils.Enter ((:~>)(Nat))

import Servant.Checked.Exceptions.Internal (Envelope, Throws, pureErrEnvelope, pureSuccEnvelope)

defaultMainApi :: IO ()
defaultMainApi = run 8201 app

type Api = ApiSearch

type ApiSearch =
  "search" :>
  QueryParam "q" String :>
  Throws FooErr :>
  Throws BarErr :>
  Post '[JSON] String

serverRoot :: ServerT Api Handler
serverRoot = search

search :: Maybe String -> Handler (Envelope '[FooErr, BarErr] String)
search maybeQ =
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
