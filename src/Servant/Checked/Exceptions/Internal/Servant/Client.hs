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

{- |
Module      :  Servant.Checked.Exceptions.Internal.Servant.Client

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module only exports 'HasClient' instances for 'Throws' and 'Throwing'.
-}

module Servant.Checked.Exceptions.Internal.Servant.Client where

import Data.Proxy (Proxy(Proxy))
import Servant.API (Verb, (:>), (:<|>))
import Servant.Client (HasClient(clientWithRoute, Client))
import Servant.Common.Req (Req)

import Servant.Checked.Exceptions.Internal.Envelope (Envelope)
import Servant.Checked.Exceptions.Internal.Servant.API
       (Throws, Throwing, ThrowingNonterminal)

-- TODO: Make sure to also account for when headers are being used.

-- | Change a 'Throws' into 'Throwing'.
instance (HasClient (Throwing '[e] :> api)) => HasClient (Throws e :> api) where
  type Client (Throws e :> api) = Client (Throwing '[e] :> api)

  clientWithRoute
    :: Proxy (Throws e :> api)
    -> Req
    -> Client (Throwing '[e] :> api)
  clientWithRoute Proxy = clientWithRoute (Proxy :: Proxy (Throwing '[e] :> api))

-- | When @'Throwing' es@ comes before a 'Verb', change it into the same 'Verb'
-- but returning an @'Envelope' es@.
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

-- | When @'Throwing' es@ comes before ':<|>', push @'Throwing' es@ into each
-- branch of the API.
instance HasClient ((Throwing es :> api1) :<|> (Throwing es :> api2)) =>
    HasClient (Throwing es :> (api1 :<|> api2)) where

  type Client (Throwing es :> (api1 :<|> api2)) =
    Client ((Throwing es :> api1) :<|> (Throwing es :> api2))

  clientWithRoute
    :: Proxy (Throwing es :> (api1 :<|> api2))
    -> Req
    -> Client ((Throwing es :> api1) :<|> (Throwing es :> api2))
  clientWithRoute _ =
    clientWithRoute (Proxy :: Proxy ((Throwing es :> api1) :<|> (Throwing es :> api2)))

-- | When a @'Throws' e@ comes immediately after a @'Throwing' es@, 'Snoc' the
-- @e@ onto the @es@. Otherwise, if @'Throws' e@ comes before any other
-- combinator, push it down so it is closer to the 'Verb'.
instance HasClient (ThrowingNonterminal (Throwing es :> api :> apis)) =>
    HasClient (Throwing es :> api :> apis) where

  type Client (Throwing es :> api :> apis) =
    Client (ThrowingNonterminal (Throwing es :> api :> apis))

  clientWithRoute
    :: Proxy (Throwing es :> api :> apis)
    -> Req
    -> Client (ThrowingNonterminal (Throwing es :> api :> apis))
  clientWithRoute _ =
    clientWithRoute (Proxy :: Proxy (ThrowingNonterminal (Throwing es :> api :> apis)))
