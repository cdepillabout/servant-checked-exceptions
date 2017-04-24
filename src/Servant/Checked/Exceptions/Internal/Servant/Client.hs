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

module Servant.Checked.Exceptions.Internal.Servant.Client where

import Data.Proxy (Proxy(Proxy))
import Servant.API (Verb, (:>))
import Servant.Client (HasClient(clientWithRoute, Client))
import Servant.Common.Req (Req)

import Servant.Checked.Exceptions.Internal.Envelope (Envelope)
import Servant.Checked.Exceptions.Internal.Servant.API
       (Throws, Throwing)
import Servant.Checked.Exceptions.Internal.Util (Snoc)

instance (HasClient (Throwing '[e] :> api)) => HasClient (Throws e :> api) where
  type Client (Throws e :> api) = Client (Throwing '[e] :> api)

  clientWithRoute
    :: Proxy (Throws e :> api)
    -> Req
    -> Client (Throwing '[e] :> api)
  clientWithRoute Proxy = clientWithRoute (Proxy :: Proxy (Throwing '[e] :> api))

instance (HasClient (Verb method status ctypes (Envelope es a))) =>
    HasClient (Throwing es :> Verb method status ctypes a) where

  type Client (Throwing es :> Verb method status ctypes a) =
    Client (Verb method status ctypes (Envelope es a))

  clientWithRoute
    :: Proxy (Throwing es :> Verb method status ctypes a)
    -> Req
    -> Client (Verb method status ctypes (Envelope es a))
  clientWithRoute Proxy =
    clientWithRoute (Proxy :: Proxy (Verb method status ctypes (Envelope es a)))

instance (HasClient (Throwing (Snoc es e) :> api)) =>
    HasClient (Throwing es :> Throws e :> api) where

  type Client (Throwing es :> Throws e :> api) =
    Client (Throwing (Snoc es e) :> api)

  clientWithRoute
    :: Proxy (Throwing es :> Throws e :> api)
    -> Req
    -> Client (Throwing (Snoc es e) :> api)
  clientWithRoute Proxy =
    clientWithRoute (Proxy :: Proxy (Throwing (Snoc es e) :> api))
