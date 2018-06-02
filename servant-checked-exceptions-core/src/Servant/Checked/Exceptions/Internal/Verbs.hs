{- |
Module      :  Servant.Checked.Exceptions.Internal.Verbs

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module defines the 'Throws' and 'Throwing' types.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Checked.Exceptions.Internal.Verbs where


import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.TypeLits (Nat)
import Network.HTTP.Types (StdMethod(DELETE, GET, PATCH, POST, PUT))

data VerbWithErr
    (method :: k1)
    (successStatusCode :: Nat)
    (contentTypes :: [*])
    (es :: [*])
    a
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
