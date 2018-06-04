module Servant.Checked.Exceptions.Verbs (
  -- *** Specialized Verbs
  -- **** HTTP 200
    GetWithErr
  , PostWithErr
  , PutWithErr
  , DeleteWithErr
  , PatchWithErr
  , VerbWithErr
  -- **** HTTP 201
  , PostCreatedWithErr
  -- **** HTTP 202
  , GetAcceptedWithErr
  , PostAcceptedWithErr
  , DeleteAcceptedWithErr
  , PatchAcceptedWithErr
  , PutAcceptedWithErr
  -- **** HTTP 203
  , GetNonAuthoritativeWithErr
  , PostNonAuthoritativeWithErr
  , DeleteNonAuthoritativeWithErr
  , PatchNonAuthoritativeWithErr
  , PutNonAuthoritativeWithErr
  -- **** HTTP 204
  , GetNoContentWithErr
  , PostNoContentWithErr
  , DeleteNoContentWithErr
  , PatchNoContentWithErr
  , PutNoContentWithErr
  -- **** HTTP 205
  , GetResetContentWithErr
  , PostResetContentWithErr
  , DeleteResetContentWithErr
  , PatchResetContentWithErr
  , PutResetContentWithErr
  -- **** HTTP 206
  , GetPartialContentWithErr
  -- * 'Envelope' response wrapper
  , module Data.WorldPeace
  )where

import Data.WorldPeace
import Servant.Checked.Exceptions.Internal.Verbs
