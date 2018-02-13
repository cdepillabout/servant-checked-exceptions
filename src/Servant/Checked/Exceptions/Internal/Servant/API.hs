{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      :  Servant.Checked.Exceptions.Internal.Servant.API

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module defines the 'Throws' and 'Throwing' types.
-}

module Servant.Checked.Exceptions.Internal.Servant.API where

import Data.Typeable (Typeable)
import GHC.Exts (Constraint)
import GHC.Generics (Generic)
import GHC.TypeLits (Nat)
import Network.HTTP.Types -- (Status, StdMethod(..))
import Servant.API ((:>))
import Servant.API.Verbs

import Servant.Checked.Exceptions.Internal.Util (Snoc)

-- | 'Throws' is used in Servant API definitions and signifies that an API will
-- throw the given error.
--
-- Here is an example of how to create an API that potentially returns a
-- 'String' as an error, or an 'Int' on success:
--
-- >>> import Servant.API (Get, JSON, (:>))
-- >>> type API = Throws String :> Get '[JSON] Int
data Throws (e :: *)

-- | 'NoThrow' is used to indicate that an API will not throw an error, but
-- that it will still return a response wrapped in a
-- 'Servant.Checked.Exceptions.Internal.Envelope.Envelope'.
--
-- ==== __Examples__
--
-- Create an API using 'NoThrow':
--
-- >>> import Servant.API (Get, JSON, (:>))
-- >>> type API = NoThrow :> Get '[JSON] Int
--
-- A servant-server handler for this type would look like the following:
--
-- @
--   apiHandler :: 'Servant.Handler' ('Servant.Checked.Exceptions.Internal.Envelope.Envelope' \'[] Int)
--   apiHandler = 'Servant.Checked.Exceptions.Internal.Envelope.pureSuccEnvelope' 3
-- @
data NoThrow

-- | This is used internally and should not be used by end-users.
data Throwing (e :: [*])

-- | Used by the 'HasServer' and 'HasClient' instances for
-- @'Throwing' es ':>' api ':>' apis@ to detect @'Throwing' es@ followed
-- immediately by @'Throws' e@.
type family ThrowingNonterminal api where
  ThrowingNonterminal (Throwing es :> Throws e :> api) =
    Throwing (Snoc es e) :> api
  ThrowingNonterminal (Throwing es :> c :> api) =
    c :> Throwing es :> api

data VerbWithErr (method :: k1) (successStatusCode :: Nat) (contentTypes :: [*]) (es :: [*]) a
  deriving (Generic, Typeable)

type GetWithErr    = VerbWithErr 'GET    200
type PostWithErr   = VerbWithErr 'POST   200
type PutWithErr    = VerbWithErr 'PUT    200
type DeleteWithErr = VerbWithErr 'DELETE 200
type PatchWithErr  = VerbWithErr 'PATCH  200

type PostCreatedWithErr = VerbWithErr 'POST 201

type GetAcceptedWithErr    = VerbWithErr 'GET 202
type PostAcceptedWithErr   = VerbWithErr 'POST 202
type DeleteAcceptedWithErr = VerbWithErr 'DELETE 202
type PatchAcceptedWithErr  = VerbWithErr 'PATCH 202
type PutAcceptedWithErr    = VerbWithErr 'PUT 202

type GetNonAuthoritativeWithErr    = VerbWithErr 'GET 203
type PostNonAuthoritativeWithErr   = VerbWithErr 'POST 203
type DeleteNonAuthoritativeWithErr = VerbWithErr 'DELETE 203
type PatchNonAuthoritativeWithErr  = VerbWithErr 'PATCH 203
type PutNonAuthoritativeWithErr    = VerbWithErr 'PUT 203

type GetNoContentWithErr    = VerbWithErr 'GET 204
type PostNoContentWithErr   = VerbWithErr 'POST 204
type DeleteNoContentWithErr = VerbWithErr 'DELETE 204
type PatchNoContentWithErr  = VerbWithErr 'PATCH 204
type PutNoContentWithErr    = VerbWithErr 'PUT 204

type GetResetContentWithErr    = VerbWithErr 'GET 205
type PostResetContentWithErr   = VerbWithErr 'POST 205
type DeleteResetContentWithErr = VerbWithErr 'DELETE 205
type PatchResetContentWithErr  = VerbWithErr 'PATCH 205
type PutResetContentWithErr    = VerbWithErr 'PUT 205

type GetPartialContentWithErr = VerbWithErr 'GET 206

class ErrStatus e where
  toErrStatus :: e -> Status

type family AllErrStatus (es :: [k]) :: Constraint where
  AllErrStatus '[] = ()
  AllErrStatus (a ': as) = (ErrStatus a, AllErrStatus as)
