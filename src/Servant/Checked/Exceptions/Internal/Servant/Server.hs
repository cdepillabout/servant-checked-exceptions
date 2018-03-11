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
import Data.Functor.Identity
import Data.Maybe
import Data.Proxy (Proxy(Proxy))
import Data.WorldPeace (OpenUnion, Union(That, This))
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
import Servant.Server.Internal.RoutingApplication
  ( Delayed
  , DelayedIO
  , RouteResult(FailFatal, Route)
  , addAcceptCheck
  , addMethodCheck
  , delayedFail
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

import Servant.Checked.Exceptions.Internal.Envelope (Envelope, envelope)
import Servant.Checked.Exceptions.Internal.Servant.API
  ( AllErrStatus
  , ErrStatus(toErrStatus)
  , NoThrow
  , Throwing
  , ThrowingNonterminal
  , Throws
  , VerbWithErr
  )

-- TODO: Make sure to also account for when headers are being used.
-- This might be hard to do:
-- https://github.com/cdepillabout/servant-checked-exceptions/issues/4

-- | Change a 'Throws' into 'Throwing'.
instance (HasServer (Throwing '[e] :> api) context) =>
    HasServer (Throws e :> api) context where

  type ServerT (Throws e :> api) m =
    ServerT (Throwing '[e] :> api) m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy (Throwing '[e] :> api)) pc nt s

  route
    :: Proxy (Throws e :> api)
    -> Context context
    -> Delayed env (ServerT (Throwing '[e] :> api) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy (Throwing '[e] :> api))

-- | When @'Throwing' es@ comes before a 'Verb', change it into the same 'Verb'
-- but returning an @'Envelope' es@.
instance (HasServer (VerbWithErr method status ctypes es a) context) =>
    HasServer (Throwing es :> Verb method status ctypes a) context where

  type ServerT (Throwing es :> Verb method status ctypes a) m =
    ServerT (VerbWithErr method status ctypes es a) m

  hoistServerWithContext _ =
    hoistServerWithContext (Proxy :: Proxy (VerbWithErr method status ctypes es a))

  route
    :: Proxy (Throwing es :> Verb method status ctypes a)
    -> Context context
    -> Delayed env
         (ServerT (VerbWithErr method status ctypes es a) Handler)
    -> Router env
  route _ =
    route
      (Proxy :: Proxy (VerbWithErr method status ctypes es a))

-- | When 'NoThrow' comes before a 'Verb', change it into the same 'Verb'
-- but returning an @'Envelope' \'[]@.
instance
    ( HasServer (VerbWithErr method status ctypes '[] a) context
    ) =>
    HasServer (NoThrow :> Verb method status ctypes a) context where

  type ServerT (NoThrow :> Verb method status ctypes a) m =
    ServerT (VerbWithErr method status ctypes '[] a) m

  hoistServerWithContext _ =
    hoistServerWithContext (Proxy :: Proxy (VerbWithErr method status ctypes '[] a))

  route
    :: Proxy (NoThrow :> Verb method status ctypes a)
    -> Context context
    -> Delayed env (ServerT (VerbWithErr method status ctypes '[] a) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy (VerbWithErr method status ctypes '[] a))

-- | When @'Throwing' es@ comes before ':<|>', push @'Throwing' es@ into each
-- branch of the API.
instance HasServer ((Throwing es :> api1) :<|> (Throwing es :> api2)) context =>
    HasServer (Throwing es :> (api1 :<|> api2)) context where

  type ServerT (Throwing es :> (api1 :<|> api2)) m =
    ServerT ((Throwing es :> api1) :<|> (Throwing es :> api2)) m

  hoistServerWithContext _ =
    hoistServerWithContext (Proxy :: Proxy ((Throwing es :> api1) :<|> (Throwing es :> api2)))

  route
    :: Proxy (Throwing es :> (api1 :<|> api2))
    -> Context context
    -> Delayed env (ServerT ((Throwing es :> api1) :<|> (Throwing es :> api2)) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy ((Throwing es :> api1) :<|> (Throwing es :> api2)))

-- | When 'NoThrow' comes before ':<|>', push 'NoThrow' into each
-- branch of the API.
instance HasServer ((NoThrow :> api1) :<|> (NoThrow :> api2)) context =>
    HasServer (NoThrow :> (api1 :<|> api2)) context where

  type ServerT (NoThrow :> (api1 :<|> api2)) m =
    ServerT ((NoThrow :> api1) :<|> (NoThrow :> api2)) m

  hoistServerWithContext _ =
    hoistServerWithContext (Proxy :: Proxy ((NoThrow :> api1) :<|> (NoThrow :> api2)))

  route
    :: Proxy (NoThrow :> (api1 :<|> api2))
    -> Context context
    -> Delayed env (ServerT ((NoThrow :> api1) :<|> (NoThrow :> api2)) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy ((NoThrow :> api1) :<|> (NoThrow :> api2)))

-- | When a @'Throws' e@ comes immediately after a @'Throwing' es@, 'Snoc' the
-- @e@ onto the @es@. Otherwise, if @'Throws' e@ comes before any other
-- combinator, push it down so it is closer to the 'Verb'.
instance HasServer (ThrowingNonterminal (Throwing es :> api :> apis)) context =>
    HasServer (Throwing es :> api :> apis) context where

  type ServerT (Throwing es :> api :> apis) m =
    ServerT (ThrowingNonterminal (Throwing es :> api :> apis)) m

  hoistServerWithContext _ =
    hoistServerWithContext (Proxy :: Proxy (ThrowingNonterminal (Throwing es :> api :> apis)))

  route
    :: Proxy (Throwing es :> api :> apis)
    -> Context context
    -> Delayed env (ServerT (ThrowingNonterminal (Throwing es :> api :> apis)) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy (ThrowingNonterminal (Throwing es :> api :> apis)))

-- | When 'NoThrow' comes before any combinator, push it down so it is closer
-- to the 'Verb'.
instance HasServer (api :> NoThrow :> apis) context =>
    HasServer (NoThrow :> api :> apis) context where

  type ServerT (NoThrow :> api :> apis) m =
    ServerT (api :> NoThrow :> apis) m

  hoistServerWithContext _ =
    hoistServerWithContext (Proxy :: Proxy (api :> NoThrow :> apis))

  route
    :: Proxy (NoThrow :> api :> apis)
    -> Context context
    -> Delayed env (ServerT (api :> NoThrow :> apis) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy (api :> NoThrow :> apis))

---------------------
-- Verb With Error --
---------------------

instance
    {-# OVERLAPPABLE #-}
    ( AllCTRender ctypes (Envelope es a)
    , AllErrStatus es
    , KnownNat successStatus
    , ReflectMethod method
    ) =>
    HasServer (VerbWithErr method successStatus ctypes es a) context where

  type ServerT (VerbWithErr method successStatus ctypes es a) m =
    m (Envelope es a)

  hoistServerWithContext _ _ nt s = nt s

  route
    :: Proxy (VerbWithErr method successStatus ctypes es a)
    -> Context context
    -> Delayed env (Handler (Envelope es a))
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
     forall ctypes a es env.
     (AllCTRender ctypes (Envelope es a), AllErrStatus es)
  => Method
  -> Status
  -> Proxy ctypes
  -> Delayed env (Handler (Envelope es a))
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

    go :: Request -> ByteString -> Envelope es a -> RouteResult Response
    go request accH envel = do
      let status = envelope getErrStatus (const successStatus) envel
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

getErrStatus :: AllErrStatus es => OpenUnion es -> Status
getErrStatus (This (Identity e)) = toErrStatus e
getErrStatus (That es) = getErrStatus es

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
      hdrs = (hContentType, LBS.toStrict contentT) : (fromMaybe [] headers)
