{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
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
       (FromJSON(parseJSON), ToJSON(toJSON), Value, (.=), object)
import Data.Aeson.Types (Parser)
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant.Server.Internal.Router (Router)
import Servant.Server.Internal.RoutingApplication (Delayed)
import Servant
       (Context, Handler, HasServer(..), JSON, Post, QueryParam, Server,
        ServerT, Verb, (:>), enter, serve)

-- This changes in servant-0.10
-- import Control.Natural ((:~>)(NT))
import Servant.Utils.Enter ((:~>)(Nat))

import Servant.Checked.Exceptions.Internal (IsMember, OpenUnion, openUnionLift)

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
search maybeQ = do
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

------------------------
-- Servant Type-Level --
------------------------

data Throws (e :: *)

data Throwing (e :: [*])

-- TODO: Make sure to also account for when headers are being used.

instance (HasServer (Throwing '[e] :> api) context) =>
    HasServer (Throws e :> api) context where

  type ServerT (Throws e :> api) m =
    ServerT (Throwing '[e] :> api) m

  route
    :: Proxy (Throws e :> api)
    -> Context context
    -> Delayed env (ServerT (Throwing '[e] :> api) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy (Throwing '[e] :> api))

instance (HasServer (Verb method status ctypes (Envelope es a)) context) =>
    HasServer (Throwing es :> Verb method status ctypes a) context where

  type ServerT (Throwing es :> Verb method status ctypes a) m =
    ServerT (Verb method status ctypes (Envelope es a)) m

  route
    :: Proxy (Throwing es :> Verb method status ctypes a)
    -> Context context
    -> Delayed env (ServerT (Verb method status ctypes (Envelope es a)) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy (Verb method status ctypes (Envelope es a)))

instance (HasServer (Throwing (Snoc es e) :> api) context) =>
    HasServer (Throwing es :> Throws e :> api) context where

  type ServerT (Throwing es :> Throws e :> api) m =
    ServerT (Throwing (Snoc es e) :> api ) m

  route
    :: Proxy (Throwing es :> Throws e :> api)
    -> Context context
    -> Delayed env (ServerT (Throwing (Snoc es e) :> api) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy (Throwing (Snoc es e) :> api))

type family Snoc (as :: [k]) (b :: k) where
  Snoc '[] b = '[b]
  Snoc (a ': as) b = (a ': Snoc as b)

------------
-- Errors --
------------

data FooErr = FooErr deriving (Eq, Read, Show)

instance ToJSON FooErr where
  toJSON :: FooErr -> Value
  toJSON = toJSON . show

data BarErr = BarErr deriving (Eq, Read, Show)

instance ToJSON BarErr where
  toJSON :: BarErr -> Value
  toJSON = toJSON . show

data BazErr = BazErr deriving (Eq, Read, Show)

instance ToJSON BazErr where
  toJSON :: BazErr -> Value
  toJSON = toJSON . show

--------------
-- Envelope --
--------------

data Envelope e a = ErrEnvelope (OpenUnion e) | SuccEnvelope a

toErrEnvelope :: IsMember e es => e -> Envelope es a
toErrEnvelope = ErrEnvelope . openUnionLift

toSuccEnvelope :: a -> Envelope e a
toSuccEnvelope = SuccEnvelope

pureErrEnvelope :: (Applicative m, IsMember e es) => e -> m (Envelope es a)
pureErrEnvelope = pure . toErrEnvelope

pureSuccEnvelope :: Applicative m => a -> m (Envelope e a)
pureSuccEnvelope = pure . toSuccEnvelope

instance (ToJSON (OpenUnion e), ToJSON a) => ToJSON (Envelope e a) where
  toJSON :: Envelope e a -> Value
  toJSON (ErrEnvelope e) = object ["err" .= e]
  toJSON (SuccEnvelope a) = object ["data" .= a]

-- | TODO: This is only a valid instance when the 'Read' instances for the types don't overlap.
instance (FromJSON (OpenUnion e), FromJSON a) => FromJSON (Envelope e a) where
  parseJSON :: Value -> Parser (Envelope e a)
  parseJSON val = undefined -- fmap This (parseJSON val) <|> fmap That (parseJSON val)


