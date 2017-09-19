{-# LANGUAGE DataKinds #-}
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

import Servant.API ((:>))

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
