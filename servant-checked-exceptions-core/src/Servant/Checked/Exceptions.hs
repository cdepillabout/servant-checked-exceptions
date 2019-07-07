{- |
Module      :  Servant.Checked.Exceptions

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module gives you the ability to specify which errors are thrown by a
Servant api.  This is done with the 'Throws' data type.  Here is an example of
creating an api that uses 'Throws':

@
  type Api =
    \"author\" 'Servant.API.:>'
    'Servant.API.Capture' \"author-id\" AuthorId 'Servant.API.:>'
    'Throws' CouldNotConnectToDbError 'Servant.API.:>'
    'Throws' AuthorNotFoundError 'Servant.API.:>'
    'Servant.API.Get' \'['Servant.API.JSON'] Author
@

This api will return an @Author@ for a given @AuthorId@.  'Throws' is used
to indicate that this api will potentially return two different errors:
@CouldNotConnectToDbError@ and @AuthorNotFoundError@.

These two errors might be defined like this:

@
  data CouldNotConnectToDbError = CouldNotConnectToDbError
    deriving ('Eq', 'Read', 'Show')

  data AuthorNotFoundError = AuthorNotFoundError
    deriving ('Eq', 'Read', 'Show')
@

Writing the server handler for this api will look like the following.  Notice
how the 'Envelope' type is used:

@
  getAuthorHandler
    :: AuthorId
    -> 'Handler' ('Envelope' \'[DatabaseError, AuthorNotFoundError] Author)
  getAuthorHandler authorId = do
    eitherAuthor <- getAuthorFromDb authorId
    case eitherAuthor of
      Left NoDb -> pure $ 'toErrEnvelope' CouldNotConnectToDbError
      Left NoAuthor -> pure $ 'toErrEnvelope' AuthorNotFoundError
      Right author -> pure $ 'toSuccEnvelope' author

  getAuthorFromDb :: AuthorId -> Handler (Either DbErr Author)
  getAuthorFromDb = ...

  data DbErr = NoDb | NoAuthor
@

@'Envelope' \'[DatabaseError, AuthorNotFoundError] Author@ represents a
response that will contain an @Author@ on success, or contain either a
@DatabaseError@ or a @AuthorNotFoundError@ on error.

Under the hood, 'Envelope' is using an extensible sum-type ('OpenUnion') to
represent possible errors.  Working with an api that returns two possible
errors is just as easy as working with an api that returns three possible
errors.

Clients will also use the 'Envelope' type:

@
  getAuthor
    :: AuthorId
    -> 'Servant.Client.ClientM' ('Envelope' \'[DatabaseError, AuthorNotFoundError] Author)
  getAuthor = 'Servant.Client.client' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' Api)
@

It is easy to do case analysis (similar to pattern matching) on the 'Envelope'
type with the 'catchesEnvelope' function.

Checkout the
<https://github.com/cdepillabout/servant-checked-exceptions/tree/master/example example>
in the repository on Github.  It includes a fleshed-out example of an
<https://github.com/cdepillabout/servant-checked-exceptions/blob/master/example/Api.hs api>,
<https://github.com/cdepillabout/servant-checked-exceptions/blob/master/example/Server.hs server>,
<https://github.com/cdepillabout/servant-checked-exceptions/blob/master/example/Client.hs client>,
and
<https://github.com/cdepillabout/servant-checked-exceptions/blob/master/example/Docs.hs documentation>.
The <https://github.com/cdepillabout/servant-checked-exceptions README.md>
shows how to compile and run the examples.
-}

module Servant.Checked.Exceptions
  (
  -- * Servant Types
  -- ** 'Throws' API parameter
    Throws
  -- ** 'NoThrow' API parameter
  , NoThrow
  -- ** HTTP Error Status Code
  , ErrStatus(toErrStatus)
  , Status
  -- ** Verbs
  , VerbWithErr
  -- *** Specialized Verbs
  -- **** HTTP 200
  , GetWithErr
  , PostWithErr
  , PutWithErr
  , DeleteWithErr
  , PatchWithErr
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
  , Envelope(..)
  -- ** 'Envelope' helper functions
  -- *** 'Envelope' constructors
  , toSuccEnvelope
  , toErrEnvelope
  , pureSuccEnvelope
  , pureErrEnvelope
  -- *** 'Envelope' destructors
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
  -- ** Other 'Envelope' combinators
  , relaxEnvelope
  , liftA2Envelope
  , bindEnvelope
  -- *** 'Envelope' optics
  , _SuccEnvelope
  , _ErrEnvelope
  , _ErrEnvelopeErr
  -- *** 'Envelope' and 'Either'
  , envelopeToEither
  , eitherToEnvelope
  , isoEnvelopeEither
  -- * 'EnvelopeT' short-circuiting monad transformer
  , EnvelopeT(..)
  -- ** 'EnvelopeT' helper functions
  -- *** 'EnvelopeT' constructors
  , pureSuccEnvT
  , throwErrEnvT
  -- ** 'EnvelopeT' destructors
  , envelopeT
  , fromEnvT
  , fromEnvTOr
  , errEnvTMatch
  , catchesEnvT
  , emptyEnvT
  , envTRemove
  , envTHandle
  -- ** Other 'EnvelopeT' combinators
  , relaxEnvT
  , liftA2EnvT
  , bindEnvT
  -- ** 'Envelope' and 'ExceptT'
  , envTToExceptT
  , exceptTToEnvT
  -- * Re-exported modules
  -- | "Data.WorldPeace" exports the 'OpenUnion' type as well as other
  -- combinators.  It also exports the 'OpenProduct' type and 'ToProduct' type
  -- class used by some of the functions above.
  , module Data.WorldPeace
  ) where

import Data.WorldPeace
import Network.HTTP.Types (Status)

import Servant.Checked.Exceptions.Internal
