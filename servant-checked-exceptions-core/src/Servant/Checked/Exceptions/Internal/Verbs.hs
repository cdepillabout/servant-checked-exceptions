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
import Servant.Checked.Exceptions.Internal.Envelope (Envelope)

data VerbWithErr'
    (method :: k1)
    (successStatusCode :: Nat)
    (contentTypes :: [*])
    (envel :: [*] -> * -> *)
    (es :: [*])
    a
  deriving (Generic, Typeable)
type VerbWithErr
    (method :: k1)
    (successStatusCode :: Nat)
    (contentTypes :: [*])
    (es :: [*])
    a
 = VerbWithErr' method successStatusCode contentTypes Envelope es a

-- (contentTypes :: [*]) (es :: [*]) a
type GetWithErr    (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'GET    200 contentTypes es a
type PostWithErr   (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'POST   200 contentTypes es a
type PutWithErr    (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'PUT    200 contentTypes es a
type DeleteWithErr (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'DELETE 200 contentTypes es a
type PatchWithErr  (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'PATCH  200 contentTypes es a

type PostCreatedWithErr (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'POST 201 contentTypes es a

type GetAcceptedWithErr    (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'GET 202 contentTypes es a
type PostAcceptedWithErr   (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'POST 202 contentTypes es a
type DeleteAcceptedWithErr (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'DELETE 202 contentTypes es a
type PatchAcceptedWithErr  (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'PATCH 202 contentTypes es a
type PutAcceptedWithErr    (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'PUT 202 contentTypes es a

type GetNonAuthoritativeWithErr    (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'GET 203 contentTypes es a
type PostNonAuthoritativeWithErr   (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'POST 203 contentTypes es a
type DeleteNonAuthoritativeWithErr (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'DELETE 203 contentTypes es a
type PatchNonAuthoritativeWithErr  (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'PATCH 203 contentTypes es a
type PutNonAuthoritativeWithErr    (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'PUT 203 contentTypes es a

type GetNoContentWithErr    (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'GET 204 contentTypes es a
type PostNoContentWithErr   (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'POST 204 contentTypes es a
type DeleteNoContentWithErr (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'DELETE 204 contentTypes es a
type PatchNoContentWithErr  (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'PATCH 204 contentTypes es a
type PutNoContentWithErr    (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'PUT 204 contentTypes es a

type GetResetContentWithErr    (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'GET 205 contentTypes es a
type PostResetContentWithErr   (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'POST 205 contentTypes es a
type DeleteResetContentWithErr (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'DELETE 205 contentTypes es a
type PatchResetContentWithErr  (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'PATCH 205 contentTypes es a
type PutResetContentWithErr    (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'PUT 205 contentTypes es a

type GetPartialContentWithErr (contentTypes :: [*]) (es :: [*]) a = VerbWithErr 'GET 206 contentTypes es a
