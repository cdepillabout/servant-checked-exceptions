{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Checked.Exceptions.Internal.Envelope where

import Control.Applicative ((<|>))
import Control.Lens (Iso, Prism, Prism', iso, preview, prism)
import Control.Monad.Fix (MonadFix(mfix))
import Data.Aeson
       (FromJSON(parseJSON), ToJSON(toJSON), Value, (.=), (.:), object,
        withObject)
import Data.Aeson.Types (Parser)
import Data.Data (Data)
import Data.Semigroup (Semigroup((<>), stimes), stimesIdempotent)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Servant.Checked.Exceptions.Internal.Union
       (IsMember, OpenUnion, openUnionLift, openUnionPrism)


-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> import Control.Lens (preview, review)
-- >>> import Data.Text (Text)
-- >>> import Text.Read (readMaybe)


-- | This 'Envelope' type is a used as a wrapper around either an 'OpenUnion'
-- with an error or a successful value.  It is similar to an @'Either' e a@,
-- but where the @e@ is specialized to @'OpenUnion' es@.  The most important
-- different from 'Either' is the the 'FromJSON' and 'ToJSON' instances.
--
-- Given an @'Envelope' \'['String', 'Double'] '()'@, we know that the envelope
-- could be a 'SuccEnvelope' and contain '()'.  Or it could be a 'ErrEnvelope'
-- that contains /either/ a 'String' /or/ a 'Double'.  It might be simpler to
-- think of it as a type like @'Either' 'String' ('Either' 'Double' '()')@.
--
-- An 'Envelope' can be created with the 'toErrEnvelope' and 'toSuccEnvelope'
-- functions.  The 'Prism's '_SuccEnvelope', '_ErrEnvelope', and
-- '_ErrEnvelopeErr' can be used to get values out of an 'Envelope'.
data Envelope es a = ErrEnvelope (OpenUnion es) | SuccEnvelope a
  deriving (Foldable, Functor, Generic, Traversable)

-- | Create an 'ErrEnvelope' from a member of the 'OpenUnion'.
--
-- For instance, here is how to create an 'ErrEnvelope' that contains a
-- 'Double':
--
-- >>> let double = 3.5 :: Double
-- >>> toErrEnvelope double :: Envelope '[String, Double, Int] ()
-- ErrEnvelope (Identity 3.5)
toErrEnvelope :: IsMember e es => e -> Envelope es a
toErrEnvelope = ErrEnvelope . openUnionLift

-- | This is a function to create a 'SuccEnvelope'.
--
-- >>> toSuccEnvelope "hello" :: Envelope '[Double] String
-- SuccEnvelope "hello"
toSuccEnvelope :: a -> Envelope es a
toSuccEnvelope = SuccEnvelope

-- | 'pureErrEnvelope' is 'toErrEnvelope' lifted up to an 'Applicative'.
pureErrEnvelope :: (Applicative m, IsMember e es) => e -> m (Envelope es a)
pureErrEnvelope = pure . toErrEnvelope

-- | 'pureSuccEnvelope' is 'toSuccEnvelope' lifted up to an 'Applicative'.
pureSuccEnvelope :: Applicative m => a -> m (Envelope es a)
pureSuccEnvelope = pure . toSuccEnvelope

-- | Case analysis for 'Envelope's.
--
--  Here is an example of matching on a 'SuccEnvelope':
--
-- >>> let env = toSuccEnvelope "hello" :: Envelope '[Double, Int] String
-- >>> envelope (const "not a String") id env
-- "hello"
--
-- Here is an example of matching on a 'ErrEnvelope':
--
-- >>> let double = 3.5 :: Double
-- >>> let env' = toErrEnvelope double :: Envelope '[Double, Int] String
-- >>> envelope (const "not a String") id env'
-- "not a String"
envelope :: (OpenUnion es -> c) -> (a -> c) -> Envelope es a -> c
envelope f _ (ErrEnvelope es) = f es
envelope _ f (SuccEnvelope a) = f a

-- | Just like 'fromEither' but for 'Envelope'.
--
--  Here is an example of successfully matching:
--
-- >>> let env = toSuccEnvelope "hello" :: Envelope '[Double, Int] String
-- >>> fromEnvelope (const "not a String") env
-- "hello"
--
-- Here is an example of unsuccessfully matching:
--
-- >>> let double = 3.5 :: Double
-- >>> let env' = toErrEnvelope double :: Envelope '[Double, Int] String
-- >>> fromEnvelope (const "not a String") env'
-- "not a String"
fromEnvelope :: (OpenUnion es -> a) -> Envelope es a -> a
fromEnvelope f = envelope f id

-- | Lifted version of 'fromEnvelope'.
fromEnvelopeM
  :: Applicative m
  => (OpenUnion es -> m a) -> Envelope es a -> m a
fromEnvelopeM f = envelope f pure

-- | Flipped version of 'fromEnvelope'.
fromEnvelopeOr :: Envelope es a -> (OpenUnion es -> a) -> a
fromEnvelopeOr = flip fromEnvelope

-- | Flipped version of 'fromEnvelopeM'.
fromEnvelopeOrM
  :: Applicative m
  => Envelope es a -> (OpenUnion es -> m a) -> m a
fromEnvelopeOrM = flip fromEnvelopeM

-- | Convert an 'Envelope' to an 'Either'.
envelopeToEither :: Envelope es a -> Either (OpenUnion es) a
envelopeToEither (ErrEnvelope es) = Left es
envelopeToEither (SuccEnvelope a) = Right a

-- | Convert an 'Either' to an 'Envelope'.
eitherToEnvelope :: Either (OpenUnion es) a -> Envelope es a
eitherToEnvelope (Left es) = ErrEnvelope es
eitherToEnvelope (Right a) = SuccEnvelope a

-- | Lens-compatible 'Iso' from 'Envelope' to 'Either'.
isoEnvelopeEither :: Iso (Envelope es a) (Envelope fs b) (Either (OpenUnion es) a) (Either (OpenUnion fs) b)
isoEnvelopeEither = iso envelopeToEither eitherToEnvelope

-- | Lens-compatible 'Prism' to pull out an @a@ from a 'SuccEnvelope'.
--
-- Use '_SuccEnvelope' to construct an 'Envelope':
--
-- >>> review _SuccEnvelope "hello" :: Envelope '[Double] String
-- SuccEnvelope "hello"
--

-- Use '_This' to try to destruct a 'Union' into a @f a@:
--
-- >>> let u = This (Identity "hello") :: Union Identity '[String, Int]
-- >>> preview _This u :: Maybe (Identity String)
-- Just (Identity "hello")
--
-- Use '_This' to try to destruct a 'Union' into a @f a@ (unsuccessfully):
--
-- >>> let v = That (This (Identity 3.3)) :: Union Identity '[String, Double, Int]
-- >>> preview _This v :: Maybe (Identity String)
-- Nothing
_SuccEnvelope :: Prism (Envelope es a) (Envelope es b) a b
_SuccEnvelope = prism SuccEnvelope $ envelope (Left . ErrEnvelope) Right

_ErrEnvelope :: Prism (Envelope es a) (Envelope es' a) (OpenUnion es) (OpenUnion es')
_ErrEnvelope = prism ErrEnvelope $ envelope Right (Left . SuccEnvelope)

_ErrEnvelopeErr :: forall e es a. IsMember e es => Prism' (Envelope es a) e
_ErrEnvelopeErr = _ErrEnvelope . openUnionPrism

errEnvelopeMatch
  :: forall e es a.
     IsMember e es
  => Envelope es a -> Maybe e
errEnvelopeMatch = preview _ErrEnvelopeErr

instance (ToJSON (OpenUnion es), ToJSON a) => ToJSON (Envelope es a) where
  toJSON :: Envelope es a -> Value
  toJSON (ErrEnvelope es) = object ["err" .= es]
  toJSON (SuccEnvelope a) = object ["data" .= a]

instance (FromJSON (OpenUnion es), FromJSON a) => FromJSON (Envelope es a) where
  parseJSON :: Value -> Parser (Envelope es a)
  parseJSON = withObject "Envelope" $ \obj ->
    SuccEnvelope <$> obj .: "data" <|>
    ErrEnvelope <$> obj .: "err"

deriving instance (Data (OpenUnion es), Data a, Typeable es) => Data (Envelope es a)
deriving instance (Eq (OpenUnion es), Eq a) => Eq (Envelope es a)
deriving instance (Ord (OpenUnion es), Ord a) => Ord (Envelope es a)
deriving instance (Read (OpenUnion es), Read a) => Read (Envelope es a)
deriving instance (Show (OpenUnion es), Show a) => Show (Envelope es a)
deriving instance (Typeable (OpenUnion es), Typeable a) => Typeable (Envelope es a)

instance Applicative (Envelope es) where
  pure :: a -> Envelope es a
  pure = SuccEnvelope

  (<*>) :: Envelope es (a -> b) -> Envelope es a -> Envelope es b
  ErrEnvelope es <*> _ = ErrEnvelope es
  SuccEnvelope f <*> r = fmap f r

instance Monad (Envelope es) where
  (>>=) :: Envelope es a -> (a -> Envelope es b) -> Envelope es b
  ErrEnvelope es >>= _ = ErrEnvelope es
  SuccEnvelope a >>= k = k a

instance MonadFix (Envelope es) where
  mfix :: (a -> Envelope es a) -> Envelope es a
  mfix f =
    let a = f (unSucc a)
    in a
    where
      unSucc :: Envelope es a -> a
      unSucc (SuccEnvelope x) = x
      unSucc (ErrEnvelope _) = errorWithoutStackTrace "mfix Envelope: ErrEnvelope"

instance Semigroup (Envelope es a) where
  (<>) :: Envelope es a -> Envelope es a -> Envelope es a
  ErrEnvelope _ <> b = b
  a      <> _ = a

  stimes :: Integral b => b -> Envelope es a -> Envelope es a
  stimes = stimesIdempotent
