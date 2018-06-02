module Servant.Checked.Exceptions.Envelope (
  -- * Envelope
    Envelope(..)
  -- * Helper functions
  -- ** Envelope Constructors
  , toSuccEnvelope
  , toErrEnvelope
  , pureSuccEnvelope
  , pureErrEnvelope
  -- ** Envelope Destructors
  , envelope
  , emptyEnvelope
  , fromEnvelope
  , fromEnvelopeOr
  , fromEnvelopeM
  , fromEnvelopeOrM
  , errEnvelopeMatch
  , catchesEnvelope
  -- ** Optics
  , _SuccEnvelope
  , _ErrEnvelope
  , _ErrEnvelopeErr
  -- ** Either
  , envelopeToEither
  , eitherToEnvelope
  , isoEnvelopeEither
  -- * Setup code for doctests
  -- $setup
  ) where

import Servant.Checked.Exceptions.Internal.Envelope
