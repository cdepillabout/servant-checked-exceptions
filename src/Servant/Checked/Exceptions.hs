module Servant.Checked.Exceptions
  (
  -- * 'Throws' API parameter
    Throws
  -- * 'Envelope' response wrapper
  , Envelope(..)
  -- ** 'Envelope' helper functions
  -- *** 'Envelope' constructors
  , toSuccEnvelope
  , toErrEnvelope
  , pureSuccEnvelope
  , pureErrEnvelope
  -- *** 'Envelope' destructors
  , envelope
  , fromEnvelope
  , fromEnvelopeOr
  , fromEnvelopeM
  , fromEnvelopeOrM
  , errEnvelopeMatch
  -- *** 'Envelope' optics
  , _SuccEnvelope
  , _ErrEnvelope
  , _ErrEnvelopeErr
  -- *** 'Envelope' and 'Either'
  , envelopeToEither
  , eitherToEnvelope
  , isoEnvelopeEither
  -- *** 'OpenUnion' (used in 'ErrEnvelope')
  , OpenUnion
  -- **** 'OpenUnion' Helpers
  , openUnion
  , fromOpenUnion
  , fromOpenUnionOr
  , openUnionPrism
  , openUnionLift
  , openUnionMatch
  -- **** 'Union' (used by 'OpenUnion')
  -- | 'OpenUnion' is a type synonym around 'Union'. Most users will be able to
  -- work directly with 'OpenUnion' and ignore this 'Union' type.
  , Union(..)
  -- ***** Union helpers
  , union
  , absurdUnion
  , umap
  -- ***** Union optics
  , _This
  , _That
  -- ***** Typeclasses used with Union
  , RIndex
  , UElem(..)
  , IsMember
  ) where

import Servant.Checked.Exceptions.Internal
