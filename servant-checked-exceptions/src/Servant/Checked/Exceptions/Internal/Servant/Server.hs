{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      :  Servant.Checked.Exceptions.Internal.Servant.Server

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module exports 'HasServer' instances for 'Throws' and 'Throwing'.
-}

module Servant.Checked.Exceptions.Internal.Servant.Server where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownNat, natVal)
import Network.HTTP.Types
import Network.Wai
import Servant.API.ContentTypes
  ( AcceptHeader(AcceptHeader)
  , AllCTRender
  , AllMime
  , canHandleAcceptH
  , handleAcceptH
  )
import Servant.Server.Internal (ct_wildcard)
import Servant.Server.Internal.Router (Router, Router', leafRouter)
import Servant.Server.Internal.RouteResult (RouteResult(FailFatal, Route))
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFail)
import Servant.Server.Internal.Delayed
  ( Delayed
  , addAcceptCheck
  , addMethodCheck
  , runAction
  )
import Servant
  ( (:<|>)(..)
  , (:>)
  , Context
  , Handler
  , HasServer(..)
  , ReflectMethod
  , ServerT
  , Verb
  , err405
  , err406
  , reflectMethod
  )

import Servant.Checked.Exceptions.Internal.Servant.API
  ( AllErrStatus
  , EnvelopeStatus(..)
  , NoThrow'
  , Throwing'
  , ThrowingNonterminal
  , Throws'
  )
import Servant.Checked.Exceptions.Verbs (VerbWithErr')

-- TODO: Make sure to also account for when headers are being used.
-- This might be hard to do:
-- https://github.com/cdepillabout/servant-checked-exceptions/issues/4

-- | Change a 'Throws' into 'Throwing'.
instance (HasServer (Throwing' envel '[e] :> api) context) =>
    HasServer (Throws' envel e :> api) context where

  type ServerT (Throws' envel e :> api) m =
    ServerT (Throwing' envel '[e] :> api) m

  hoistServerWithContext _ =
    hoistServerWithContext (Proxy :: Proxy (Throwing' envel '[e] :> api))

  route
    :: Proxy (Throws' envel e :> api)
    -> Context context
    -> Delayed env (ServerT (Throwing' envel '[e] :> api) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy (Throwing' envel '[e] :> api))

-- | When @'Throwing' es@ comes before a 'Verb', change it into the same 'Verb'
-- but returning an @'Envelope' es@.
instance (HasServer (VerbWithErr' method status ctypes envel es a) context) =>
    HasServer (Throwing' envel es :> Verb method status ctypes a) context where

  type ServerT (Throwing' envel es :> Verb method status ctypes a) m =
    ServerT (VerbWithErr' method status ctypes envel es a) m

  hoistServerWithContext _ =
    hoistServerWithContext (Proxy :: Proxy (VerbWithErr' method status ctypes envel es a))

  route
    :: Proxy (Throwing' envel es :> Verb method status ctypes a)
    -> Context context
    -> Delayed env
         (ServerT (VerbWithErr' method status ctypes envel es a) Handler)
    -> Router env
  route _ =
    route
      (Proxy :: Proxy (VerbWithErr' method status ctypes envel es a))

-- | When 'NoThrow' comes before a 'Verb', change it into the same 'Verb'
-- but returning an @'Envelope' \'[]@.
instance
    ( HasServer (VerbWithErr' method status ctypes envel '[] a) context
    ) =>
    HasServer (NoThrow' envel :> Verb method status ctypes a) context where

  type ServerT (NoThrow' envel :> Verb method status ctypes a) m =
    ServerT (VerbWithErr' method status ctypes envel '[] a) m

  hoistServerWithContext _ =
    hoistServerWithContext (Proxy :: Proxy (VerbWithErr' method status ctypes envel '[] a))

  route
    :: Proxy (NoThrow' envel :> Verb method status ctypes a)
    -> Context context
    -> Delayed env (ServerT (VerbWithErr' method status ctypes envel '[] a) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy (VerbWithErr' method status ctypes envel '[] a))

-- | When @'Throwing' es@ comes before ':<|>', push @'Throwing' es@ into each
-- branch of the API.
instance HasServer ((Throwing' envel es :> api1) :<|> (Throwing' envel es :> api2)) context =>
    HasServer (Throwing' envel es :> (api1 :<|> api2)) context where

  type ServerT (Throwing' envel es :> (api1 :<|> api2)) m =
    ServerT ((Throwing' envel es :> api1) :<|> (Throwing' envel es :> api2)) m

  hoistServerWithContext _ =
    hoistServerWithContext (Proxy :: Proxy ((Throwing' envel es :> api1) :<|> (Throwing' envel es :> api2)))

  route
    :: Proxy (Throwing' envel es :> (api1 :<|> api2))
    -> Context context
    -> Delayed env (ServerT ((Throwing' envel es :> api1) :<|> (Throwing' envel es :> api2)) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy ((Throwing' envel es :> api1) :<|> (Throwing' envel es :> api2)))

-- | When 'NoThrow' comes before ':<|>', push 'NoThrow' into each
-- branch of the API.
instance HasServer ((NoThrow' envel :> api1) :<|> (NoThrow' envel :> api2)) context =>
    HasServer (NoThrow' envel :> (api1 :<|> api2)) context where

  type ServerT (NoThrow' envel :> (api1 :<|> api2)) m =
    ServerT ((NoThrow' envel :> api1) :<|> (NoThrow' envel :> api2)) m

  hoistServerWithContext _ =
    hoistServerWithContext (Proxy :: Proxy ((NoThrow' envel :> api1) :<|> (NoThrow' envel :> api2)))

  route
    :: Proxy (NoThrow' envel :> (api1 :<|> api2))
    -> Context context
    -> Delayed env (ServerT ((NoThrow' envel :> api1) :<|> (NoThrow' envel :> api2)) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy ((NoThrow' envel :> api1) :<|> (NoThrow' envel :> api2)))

-- | When a @'Throws' e@ comes immediately after a @'Throwing' es@, 'Snoc' the
-- @e@ onto the @es@. Otherwise, if @'Throws' e@ comes before any other
-- combinator, push it down so it is closer to the 'Verb'.
instance HasServer (ThrowingNonterminal (Throwing' envel es :> api :> apis)) context =>
    HasServer (Throwing' envel es :> api :> apis) context where

  type ServerT (Throwing' envel es :> api :> apis) m =
    ServerT (ThrowingNonterminal (Throwing' envel es :> api :> apis)) m

  hoistServerWithContext _ =
    hoistServerWithContext (Proxy :: Proxy (ThrowingNonterminal (Throwing' envel es :> api :> apis)))

  route
    :: Proxy (Throwing' envel es :> api :> apis)
    -> Context context
    -> Delayed env (ServerT (ThrowingNonterminal (Throwing' envel es :> api :> apis)) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy (ThrowingNonterminal (Throwing' envel es :> api :> apis)))

-- | When 'NoThrow' comes before any combinator, push it down so it is closer
-- to the 'Verb'.
instance HasServer (api :> NoThrow' envel :> apis) context =>
    HasServer (NoThrow' envel :> api :> apis) context where

  type ServerT (NoThrow' envel :> api :> apis) m =
    ServerT (api :> NoThrow' envel :> apis) m

  hoistServerWithContext _ =
    hoistServerWithContext (Proxy :: Proxy (api :> NoThrow' envel :> apis))

  route
    :: Proxy (NoThrow' envel :> api :> apis)
    -> Context context
    -> Delayed env (ServerT (api :> NoThrow' envel :> apis) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy (api :> NoThrow' envel :> apis))

---------------------
-- Verb With Error --
---------------------

instance
    {-# OVERLAPPABLE #-}
    ( AllCTRender ctypes (envel es a)
    , AllErrStatus es
    , KnownNat successStatus
    , ReflectMethod method
    , EnvelopeStatus es envel
    ) =>
    HasServer (VerbWithErr' method successStatus ctypes envel es a) context where

  type ServerT (VerbWithErr' method successStatus ctypes envel es a) m =
    m (envel es a)

  hoistServerWithContext _ _ nt = nt

  route
    :: Proxy (VerbWithErr' method successStatus ctypes envel es a)
    -> Context context
    -> Delayed env (Handler (envel es a))
    -> Router' env
         ( Request ->
           (RouteResult Response -> IO ResponseReceived) ->
           IO ResponseReceived
         )
  route Proxy _ = methodRouter method successStatus (Proxy :: Proxy ctypes)
    where
      method :: Method
      method = reflectMethod (Proxy :: Proxy method)

      successStatus :: Status
      successStatus =
        toEnum . fromInteger $ natVal (Proxy :: Proxy successStatus)

methodRouter ::
     forall ctypes a envel es env.
     (AllCTRender ctypes (envel es a), AllErrStatus es, EnvelopeStatus es envel)
  => Method
  -> Status
  -> Proxy ctypes
  -> Delayed env (Handler (envel es a))
  -> Router' env
       ( Request ->
         (RouteResult Response -> IO ResponseReceived)->
         IO ResponseReceived
       )
methodRouter method successStatus proxy action = leafRouter route'
  where
    route'
      :: env
      -> Request
      -> (RouteResult Response -> IO ResponseReceived)
      -> IO ResponseReceived
    route' env request respond = do
      let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
      let theAction =
            action
              `addMethodCheck` methodCheck method request
              `addAcceptCheck` acceptCheck proxy accH
      runAction theAction env request respond $ go request accH

    go :: Request -> ByteString -> envel es a -> RouteResult Response
    go request accH envel = do
      let status = getEnvelopeStatus envel successStatus
      let handleA = handleAcceptH proxy (AcceptHeader accH) envel
      processMethodRouter handleA status method Nothing request

allowedMethod :: Method -> Request -> Bool
allowedMethod method request =
  allowedMethodHead method request || requestMethod request == method

allowedMethodHead :: Method -> Request -> Bool
allowedMethodHead method request =
  method == methodGet && requestMethod request == methodHead

methodCheck :: Method -> Request -> DelayedIO ()
methodCheck method request
  | allowedMethod method request = return ()
  | otherwise                    = delayedFail err405

acceptCheck :: (AllMime list) => Proxy list -> ByteString -> DelayedIO ()
acceptCheck proxy accH
  | canHandleAcceptH proxy (AcceptHeader accH) = return ()
  | otherwise                                  = delayedFail err406

processMethodRouter
  :: Maybe (LBS.ByteString, LBS.ByteString)
  -> Status
  -> Method
  -> Maybe [(HeaderName, ByteString)]
  -> Request -> RouteResult Response
processMethodRouter handleA status method headers request = case handleA of
  Nothing -> FailFatal err406 -- this should not happen (checked before),
                              -- so we make it fatal if it does
  Just (contentT, body) -> Route $ responseLBS status hdrs bdy
    where
      bdy = if allowedMethodHead method request then "" else body
      hdrs = (hContentType, LBS.toStrict contentT) : fromMaybe [] headers
