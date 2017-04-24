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

module Servant.Checked.Exceptions.Internal.Servant.Docs where

import Control.Lens ((&), (<>~))
import Data.Proxy (Proxy(Proxy))
import Servant.API (Verb, (:>))
import Servant.Docs (Action, API, DocOptions, Endpoint, HasDocs(docsFor))
import Servant.Docs.Internal (DocNote(DocNote), notes)

import Servant.Checked.Exceptions.Internal.Envelope (Envelope)
import Servant.Checked.Exceptions.Internal.Servant.API
       (Throws, Throwing)
import Servant.Checked.Exceptions.Internal.Util (Snoc)

instance (HasDocs (Throwing '[e] :> api)) => HasDocs (Throws e :> api) where
  docsFor
    :: Proxy (Throws e :> api)
    -> (Endpoint, Action)
    -> DocOptions
    -> API
  docsFor Proxy = docsFor (Proxy :: Proxy (Throwing '[e] :> api))

-- instance (HasDocs (Verb method status ctypes (Envelope es a))) =>
--     HasDocs (Throwing es :> Verb method status ctypes a) where
--   docsFor
--     :: Proxy (Throwing es :> Verb method status ctypes a)
--     -> (Endpoint, Action)
--     -> DocOptions
--     -> API
--   docsFor Proxy =
--     docsFor (Proxy :: Proxy (Verb method status ctypes (Envelope es a)))

-- TODO: Figure out what to do with the Envelope type here.
instance (HasDocs (Verb method status ctypes a)) =>
    HasDocs (Throwing es :> Verb method status ctypes a) where
  docsFor
    :: Proxy (Throwing es :> Verb method status ctypes a)
    -> (Endpoint, Action)
    -> DocOptions
    -> API
  docsFor Proxy (endpoint, action) =
    docsFor (Proxy :: Proxy (Verb method status ctypes a)) (endpoint, action')
    where
      action' :: Action
      action' = action & notes <>~ [DocNote "my title" ["my body1", "my body2", "my body3"]]

instance (HasDocs (Throwing (Snoc es e) :> api)) =>
    HasDocs (Throwing es :> Throws e :> api) where
  docsFor
    :: Proxy (Throwing es :> Throws e :> api)
    -> (Endpoint, Action)
    -> DocOptions
    -> API
  docsFor Proxy =
    docsFor (Proxy :: Proxy (Throwing (Snoc es e) :> api))
