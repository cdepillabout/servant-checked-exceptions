{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      :  Servant.Checked.Exceptions.Internal.Servant.Docs

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module exports 'HasDocs' instances for 'Throws' and 'Throwing'.
-}

module Servant.Checked.Exceptions.Internal.Servant.Docs where

import Data.Proxy (Proxy(Proxy))
import Data.ByteString.Lazy (ByteString)
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.HTTP.Media (MediaType)
import Servant.API (Verb, (:>))
import Servant.API.ContentTypes (AllMimeRender(allMimeRender))
import Servant.Docs
       (Action, API, DocOptions, Endpoint, HasDocs(docsFor),
        ToSample(toSamples))
import Servant.Docs.Internal (apiEndpoints, respBody, response)

import Servant.Checked.Exceptions.Internal.Prism ((<>~))
import Servant.Checked.Exceptions.Internal.Servant.API
       (NoThrow', Throws', Throwing', MkEnvelope(..))
import Servant.Checked.Exceptions.Internal.Util (Snoc)

-- TODO: Make sure to also account for when headers are being used.

-- | Change a 'Throws' into 'Throwing'.
instance (HasDocs (Throwing' envel '[e] :> api)) => HasDocs (Throws' envel e :> api) where
  docsFor
    :: Proxy (Throws' envel e :> api)
    -> (Endpoint, Action)
    -> DocOptions
    -> API
  docsFor Proxy = docsFor (Proxy :: Proxy (Throwing' envel '[e] :> api))

-- | When @'Throwing' es@ comes before a 'Verb', generate the documentation for
-- the same 'Verb', but returning an @'Envelope' es@.  Also add documentation
-- for the potential @es@.
instance
       ( CreateRespBodiesFor envel es ctypes
       , HasDocs (Verb method status ctypes (envel es a))
       )
    => HasDocs (Throwing' envel es :> Verb method status ctypes a) where
  docsFor
    :: Proxy (Throwing' envel es :> Verb method status ctypes a)
    -> (Endpoint, Action)
    -> DocOptions
    -> API
  docsFor Proxy (endpoint, action) docOpts =
    let api =
          docsFor
            (Proxy :: Proxy (Verb method status ctypes (envel es a)))
            (endpoint, action)
            docOpts
    in api & apiEndpoints . traverse . response . respBody <>~
        createRespBodiesFor (Proxy :: Proxy envel) (Proxy :: Proxy es) (Proxy :: Proxy ctypes)

-- | When 'NoThrow' comes before a 'Verb', generate the documentation for
-- the same 'Verb', but returning an @'Envelope' \'[]@.
instance (HasDocs (Verb method status ctypes (envel '[] a)))
    => HasDocs (NoThrow' envel :> Verb method status ctypes a) where
  docsFor
    :: Proxy (NoThrow' envel :> Verb method status ctypes a)
    -> (Endpoint, Action)
    -> DocOptions
    -> API
  docsFor Proxy (endpoint, action) docOpts =
    docsFor
      (Proxy :: Proxy (Verb method status ctypes (envel '[] a)))
      (endpoint, action)
      docOpts

-- | When a @'Throws' e@ comes immediately after a @'Throwing' es@, 'Snoc' the
-- @e@ onto the @es@.
instance (HasDocs (Throwing' envel (Snoc es e) :> api)) =>
    HasDocs (Throwing' envel es :> Throws' envel e :> api) where
  docsFor
    :: Proxy (Throwing' envel es :> Throws' envel e :> api)
    -> (Endpoint, Action)
    -> DocOptions
    -> API
  docsFor Proxy =
    docsFor (Proxy :: Proxy (Throwing' envel (Snoc es e) :> api))

-- | Create samples for an envelope with a @list@ of types, under given @ctypes@.
--
-- Instances of this class are only necessary when using a custom @envel@.
class CreateRespBodiesFor (envel :: [*] -> * -> *) list ctypes where
  createRespBodiesFor
    :: Proxy envel
    -> Proxy list
    -> Proxy ctypes
    -> [(Text, MediaType, ByteString)]

-- | An empty list of types has no samples.
instance CreateRespBodiesFor (envel :: [*] -> * -> *) '[] ctypes where
  createRespBodiesFor
    :: Proxy envel
    -> Proxy '[]
    -> Proxy ctypes
    -> [(Text, MediaType, ByteString)]
  createRespBodiesFor Proxy Proxy Proxy = []

-- | Create a response body for each of the error types.
instance
       ( AllMimeRender ctypes (envel '[e] ())
       , CreateRespBodiesFor envel es ctypes
       , ToSample e
       , MkEnvelope envel
       )
    => CreateRespBodiesFor (envel :: [*] -> * -> *) (e ': es) ctypes where
  createRespBodiesFor
    :: Proxy envel
    -> Proxy (e ': es)
    -> Proxy ctypes
    -> [(Text, MediaType, ByteString)]
  createRespBodiesFor Proxy Proxy ctypes =
    createRespBodyFor (Proxy :: Proxy envel) (Proxy :: Proxy e) ctypes <>
    createRespBodiesFor (Proxy :: Proxy envel) (Proxy :: Proxy es) ctypes

-- | Create a sample for a given @e@ under given @ctypes@.
createRespBodyFor
  :: forall (envel :: [*] -> * -> *) e ctypes.
     (AllMimeRender ctypes (envel '[e] ()), ToSample e, MkEnvelope envel)
  => Proxy envel -> Proxy e -> Proxy ctypes -> [(Text, MediaType, ByteString)]
createRespBodyFor Proxy Proxy ctypes = concatMap enc samples
    where
      samples :: [(Text, envel '[e] ())]
      samples = fmap mkErrEnvelope <$> toSamples (Proxy :: Proxy e)

      enc :: (Text, envel '[e] ()) -> [(Text, MediaType, ByteString)]
      enc (t, s) = uncurry (t,,) <$> allMimeRender ctypes s

-- | We can generate a sample of an @'Envelope' es a@ as long as there is a way
-- to generate a sample of the @a@.
--
-- This doesn't need to worry about generating a sample of @es@, because that is
-- taken care of in the 'HasDocs' instance for @'Throwing' es@.
instance (ToSample a, MkEnvelope envel) => ToSample ((envel :: [*] -> * -> *) es a) where
  toSamples :: Proxy (envel es a) -> [(Text, envel es a)]
  toSamples Proxy = fmap mkSuccEnvelope <$> toSamples (Proxy :: Proxy a)
