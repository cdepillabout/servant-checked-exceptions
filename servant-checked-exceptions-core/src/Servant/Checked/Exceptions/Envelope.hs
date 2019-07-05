module Servant.Checked.Exceptions.Envelope (
  -- * Envelope
    Envelope(..)
  -- * Helper functions
  -- ** Envelope constructors
  , toSuccEnvelope
  , toErrEnvelope
  , pureSuccEnvelope
  , pureErrEnvelope
  -- ** Envelope destructors
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
  -- * EnvelopeT
  , EnvelopeT(..)
  -- ** EnvelopeT constructors
  , pureSuccEnvT
  , throwErrEnvT
  -- ** ExceptT
  , envTToExceptT
  , exceptTToEnvT
  -- * Setup code for doctests
  -- $setup
  ) where

import Servant.Checked.Exceptions.Internal
