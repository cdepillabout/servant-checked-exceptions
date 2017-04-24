-- {-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Checked.Exceptions.Internal.Servant.Client where

import Data.Proxy (Proxy(Proxy))
import Servant.API ((:>))
import Servant.Client (HasClient(clientWithRoute, Client))
import Servant.Common.Req (Req)

import Servant.Checked.Exceptions.Internal.Servant.API
       (Throws, Throwing)


-- instance OVERLAPPABLE_
--   (MimeUnrender ct a, ReflectMethod method, cts' ~ (ct ': cts))
--   => HasClient (Verb method status cts' a) where
--   type Client (Verb method status cts' a) = ClientM a
--   clientWithRoute Proxy req = do
--     snd <$> performRequestCT (Proxy :: Proxy ct) method req
--       where method = reflectMethod (Proxy :: Proxy method)


instance (HasClient (Throwing '[e] :> api)) => HasClient (Throws e :> api) where
  type Client (Throws e :> api) = Client (Throwing '[e] :> api)

  clientWithRoute :: Proxy (Throws e :> api) -> Req -> Client (Throwing '[e] :> api)
  clientWithRoute = undefined

-- instance (HasClient (Verb method status ctypes (Envelope es a)) context) =>
--     HasClient (Throwing es :> Verb method status ctypes a) context where

--   type ServerT (Throwing es :> Verb method status ctypes a) m =
--     ServerT (Verb method status ctypes (Envelope es a)) m

--   route
--     :: Proxy (Throwing es :> Verb method status ctypes a)
--     -> Context context
--     -> Delayed env (ServerT (Verb method status ctypes (Envelope es a)) Handler)
--     -> Router env
--   route _ = route (Proxy :: Proxy (Verb method status ctypes (Envelope es a)))

-- instance (HasClient (Throwing (Snoc es e) :> api) context) =>
--     HasClient (Throwing es :> Throws e :> api) context where

--   type ServerT (Throwing es :> Throws e :> api) m =
--     ServerT (Throwing (Snoc es e) :> api ) m

--   route
--     :: Proxy (Throwing es :> Throws e :> api)
--     -> Context context
--     -> Delayed env (ServerT (Throwing (Snoc es e) :> api) Handler)
--     -> Router env
--   route _ = route (Proxy :: Proxy (Throwing (Snoc es e) :> api))

-- type family Snoc (as :: [k]) (b :: k) where
--   Snoc '[] b = '[b]
--   Snoc (a ': as) b = (a ': Snoc as b)
