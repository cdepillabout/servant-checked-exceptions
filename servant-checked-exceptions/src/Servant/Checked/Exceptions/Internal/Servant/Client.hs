{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Servant.Client.Core

import Servant.Checked.Exceptions.Internal.Servant.API
       (NoThrow', Throws', Throwing', ThrowingNonterminal)

-- TODO: Make sure to also account for when headers are being used.

-- | Change a 'Throws' into 'Throwing'.
instance (RunClient m, HasClient m (Throwing' envel '[e] :> api)) => HasClient m (Throws' envel e :> api) where
  type Client m (Throws' envel e :> api) = Client m (Throwing' envel '[e] :> api)

  clientWithRoute
    :: Proxy m
    -> Proxy (Throws' envel e :> api)
    -> Request
    -> Client m (Throwing' envel '[e] :> api)
  clientWithRoute p Proxy = clientWithRoute p (Proxy @(Throwing' envel '[e] :> api))

  hoistClientMonad
    :: Proxy m
    -> Proxy (Throws' envel e :> api)
    -> (forall x. mon x -> mon' x)
    -> Client mon (Throws' envel e :> api)
    -> Client mon' (Throwing' envel '[e] :> api)
  hoistClientMonad pm _ = hoistClientMonad pm (Proxy @(Throwing' envel '[e] :> api))


-- | When @'Throwing' es@ comes before a 'Verb', change it into the same 'Verb'
-- but returning an @'Envelope' es@.
instance (HasClient m (Verb method status ctypes (envel es a))) =>
    HasClient m (Throwing' envel es :> Verb method status ctypes a) where

  type Client m (Throwing' envel es :> Verb method status ctypes a) =
    Client m (Verb method status ctypes (envel es a))

  clientWithRoute
    :: Proxy m
    -> Proxy (Throwing' envel es :> Verb method status ctypes a)
    -> Request
    -> Client m (Verb method status ctypes (envel es a))
  clientWithRoute p Proxy =
    clientWithRoute p (Proxy :: Proxy (Verb method status ctypes (envel es a)))

  hoistClientMonad
    :: Proxy m
    -> Proxy (Throwing' envel es :> Verb method status ctypes a)
    -> (forall x. mon x -> mon' x)
    -> Client mon (Throwing' envel es :> Verb method status ctypes a)
    -> Client mon' (Verb method status ctypes (envel es a))
  hoistClientMonad pm _ =
    hoistClientMonad pm (Proxy @(Verb method status ctypes (envel es a)))



-- | When 'NoThrow' comes before a 'Verb', change it into the same 'Verb'
-- but returning an @'Envelope' \'[]@.
instance (RunClient m, HasClient m (Verb method status ctypes (envel '[] a))) =>
    HasClient m (NoThrow' envel :> Verb method status ctypes a) where

  type Client m (NoThrow' envel :> Verb method status ctypes a) =
    Client m (Verb method status ctypes (envel '[] a))

  clientWithRoute
    :: Proxy m
    -> Proxy (NoThrow' envel :> Verb method status ctypes a)
    -> Request
    -> Client m (Verb method status ctypes (envel '[] a))
  clientWithRoute p Proxy =
    clientWithRoute p (Proxy :: Proxy (Verb method status ctypes (envel '[] a)))

  hoistClientMonad
    :: Proxy m
    -> Proxy (NoThrow' envel :> Verb method status ctypes a)
    -> (forall x. mon x -> mon' x)
    -> Client mon (NoThrow' envel :> Verb method status ctypes a)
    -> Client mon' (Verb method status ctypes (envel '[] a))
  hoistClientMonad pm _ =
    hoistClientMonad pm (Proxy @(Verb method status ctypes (envel '[] a)))


-- | When @'Throwing' es@ comes before ':<|>', push @'Throwing' es@ into each
-- branch of the API.
instance (RunClient m, HasClient m ((Throwing' envel es :> api1) :<|> (Throwing' envel es :> api2))) =>
    HasClient m (Throwing' envel es :> (api1 :<|> api2)) where

  type Client m (Throwing' envel es :> (api1 :<|> api2)) =
    Client m ((Throwing' envel es :> api1) :<|> (Throwing' envel es :> api2))

  clientWithRoute
    :: Proxy m
    -> Proxy (Throwing' envel es :> (api1 :<|> api2))
    -> Request
    -> Client m ((Throwing' envel es :> api1) :<|> (Throwing' envel es :> api2))
  clientWithRoute p _ =
    clientWithRoute p (Proxy :: Proxy ((Throwing' envel es :> api1) :<|> (Throwing' envel es :> api2)))

  hoistClientMonad
    :: Proxy m
    -> Proxy (Throwing' envel es :> (api1 :<|> api2))
    -> (forall x. mon x -> mon' x)
    -> Client mon (Throwing' envel es :> (api1 :<|> api2))
    -> Client mon' ((Throwing' envel es :> api1) :<|> (Throwing' envel es :> api2))
  hoistClientMonad pm _ =
    hoistClientMonad pm (Proxy @(Throwing' envel es :> (api1 :<|> api2)))


-- | When 'NoThrow' comes before ':<|>', push 'NoThrow' into each branch of the
-- API.
instance (RunClient m, HasClient m ((NoThrow' envel :> api1) :<|> (NoThrow' envel :> api2))) =>
    HasClient m (NoThrow' envel :> (api1 :<|> api2)) where

  type Client m (NoThrow' envel :> (api1 :<|> api2)) =
    Client m ((NoThrow' envel :> api1) :<|> (NoThrow' envel :> api2))

  clientWithRoute
    :: Proxy m
    -> Proxy (NoThrow' envel :> (api1 :<|> api2))
    -> Request
    -> Client m ((NoThrow' envel :> api1) :<|> (NoThrow' envel :> api2))
  clientWithRoute p _ =
    clientWithRoute p (Proxy :: Proxy ((NoThrow' envel :> api1) :<|> (NoThrow' envel :> api2)))

  hoistClientMonad
    :: Proxy m
    -> Proxy (NoThrow' envel :> (api1 :<|> api2))
    -> (forall x. mon x -> mon' x)
    -> Client mon (NoThrow' envel :> (api1 :<|> api2))
    -> Client mon' ((NoThrow' envel :> api1) :<|> (NoThrow' envel :> api2))
  hoistClientMonad pm _ =
    hoistClientMonad pm (Proxy @(NoThrow' envel :> (api1 :<|> api2)))


-- | When a @'Throws' e@ comes immediately after a @'Throwing' es@, 'Snoc' the
-- @e@ onto the @es@. Otherwise, if @'Throws' e@ comes before any other
-- combinator, push it down so it is closer to the 'Verb'.
instance (RunClient m, HasClient m (ThrowingNonterminal (Throwing' envel es :> api :> apis))) =>
    HasClient m (Throwing' envel es :> api :> apis) where

  type Client m (Throwing' envel es :> api :> apis) =
    Client m (ThrowingNonterminal (Throwing' envel es :> api :> apis))

  clientWithRoute
    :: Proxy m
    -> Proxy (Throwing' envel es :> api :> apis)
    -> Request
    -> Client m (ThrowingNonterminal (Throwing' envel es :> api :> apis))
  clientWithRoute p _ =
    clientWithRoute p (Proxy :: Proxy (ThrowingNonterminal (Throwing' envel es :> api :> apis)))

  hoistClientMonad
    :: Proxy m
    -> Proxy (Throwing' envel es :> api :> apis)
    -> (forall x. mon x -> mon' x)
    -> Client mon (Throwing' envel es :> api :> apis)
    -> Client mon' (ThrowingNonterminal (Throwing' envel es :> api :> apis))
  hoistClientMonad pm _ =
    hoistClientMonad pm (Proxy @(ThrowingNonterminal (Throwing' envel es :> api :> apis)))


-- | When 'NoThrow' comes before any other combinator, push it down so it is
-- closer to the 'Verb'.
instance (RunClient m, HasClient m (api :> NoThrow' envel :> apis)) =>
    HasClient m (NoThrow' envel :> api :> apis) where

  type Client m (NoThrow' envel :> api :> apis) =
    Client m (api :> NoThrow' envel :> apis)

  clientWithRoute
    :: Proxy m
    -> Proxy (NoThrow' envel :> api :> apis)
    -> Request
    -> Client m (api :> NoThrow' envel :> apis)
  clientWithRoute p _ =
    clientWithRoute p (Proxy :: Proxy (api :> NoThrow' envel :> apis))

  hoistClientMonad
    :: Proxy m
    -> Proxy (NoThrow' envel :> api :> apis)
    -> (forall x. mon x -> mon' x)
    -> Client mon (NoThrow' envel :> api :> apis)
    -> Client mon' (api :> NoThrow' envel :> apis)
  hoistClientMonad pm _ =
    hoistClientMonad pm (Proxy @(api :> NoThrow' envel :> apis))
