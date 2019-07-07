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
  , envelopeRemove
  , envelopeHandle
  -- ** Other Envelope combinators
  , relaxEnvelope
  , liftA2Envelope
  , bindEnvelope
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
  -- ** EnvelopeT destructors
  , envelopeT
  , fromEnvT
  , fromEnvTOr
  , errEnvTMatch
  , catchesEnvT
  , emptyEnvT
  , envTRemove
  , envTHandle
  -- ** Other EnvelopeT combinators
  , relaxEnvT
  , liftA2EnvT
  , bindEnvT
  -- ** ExceptT
  , envTToExceptT
  , exceptTToEnvT
  -- * Setup code for doctests
  -- $setup
  ) where

import Servant.Checked.Exceptions.Internal
