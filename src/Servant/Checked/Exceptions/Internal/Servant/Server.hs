{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Checked.Exceptions.Internal.Servant.Server where

import Data.Proxy (Proxy(Proxy))
import Servant.Server.Internal.Router (Router)
import Servant.Server.Internal.RoutingApplication (Delayed)
import Servant
       (Context, Handler, HasServer(..), ServerT, Verb, (:>))

import Servant.Checked.Exceptions.Internal.Envelope (Envelope)
import Servant.Checked.Exceptions.Internal.Servant.API
       (Throws, Throwing)

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
