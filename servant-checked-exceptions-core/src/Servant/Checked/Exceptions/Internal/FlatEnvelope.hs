{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      :  Servant.Checked.Exceptions.Internal.Servant.API

This module defines the 'FlatEnvelope' type.
-}

module Servant.Checked.Exceptions.Internal.FlatEnvelope
  ( FlatEnvelope(..))
  where

import Control.Applicative ((<|>))
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.WorldPeace (OpenUnion)

import Servant.Checked.Exceptions.Internal.Servant.API (EnvelopeStatus(..), AllErrStatus, MkEnvelope(..))
import Servant.Checked.Exceptions.Internal.Envelope (Envelope(..), envelope, toErrEnvelope, toSuccEnvelope)

-- | Wrapper around @Envelope@ that has a flat JSON representation.
-- While with @Envelope@ the data and errors are contained in "err" and "data"
-- fields, with @FlatEnvelope@ they are both contained in the root dictionary.
data FlatEnvelope (es :: [*]) (succ :: *) =
  FlatEnvelope
    { unFlatEnvelope :: Envelope es succ
    }

instance (Show (OpenUnion es), Show a) => Show (FlatEnvelope es a) where
  show = envelope show show . unFlatEnvelope

-- | Both the error and success values are in the top level of the json.
instance (ToJSON (OpenUnion es), ToJSON a) => ToJSON (FlatEnvelope es a) where
  toJSON = envelope toJSON toJSON . unFlatEnvelope

-- | Both the error and success values are in the top level of the json.
-- Success values are tried first, then errors.
instance (FromJSON (OpenUnion es), FromJSON a) => FromJSON (FlatEnvelope es a) where
  parseJSON v = FlatEnvelope <$>
    (  SuccEnvelope <$> parseJSON v
   <|> ErrEnvelope  <$> parseJSON v
    )

instance AllErrStatus es => EnvelopeStatus es FlatEnvelope where
  getEnvelopeStatus (FlatEnvelope envel) = getEnvelopeStatus envel

instance MkEnvelope FlatEnvelope where
  mkSuccEnvelope = FlatEnvelope . toSuccEnvelope
  mkErrEnvelope = FlatEnvelope . toErrEnvelope
