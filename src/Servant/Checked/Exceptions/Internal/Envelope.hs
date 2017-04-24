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
import Control.Lens (Prism, Prism', preview, prism)
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

data Envelope es a = ErrEnvelope (OpenUnion es) | SuccEnvelope a
  deriving (Foldable, Functor, Generic, Traversable)

toErrEnvelope :: IsMember e es => e -> Envelope es a
toErrEnvelope = ErrEnvelope . openUnionLift

toSuccEnvelope :: a -> Envelope es a
toSuccEnvelope = SuccEnvelope

pureErrEnvelope :: (Applicative m, IsMember e es) => e -> m (Envelope es a)
pureErrEnvelope = pure . toErrEnvelope

pureSuccEnvelope :: Applicative m => a -> m (Envelope es a)
pureSuccEnvelope = pure . toSuccEnvelope

-- | Case analysis for 'Envelope's.
envelope :: (OpenUnion es -> c) -> (a -> c) -> Envelope es a -> c
envelope f _ (ErrEnvelope es) = f es
envelope _ f (SuccEnvelope a) = f a

-- | Just like 'fromEither' but for 'Envelope'.
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

envelopeToEither :: Envelope es a -> Either (OpenUnion es) a
envelopeToEither (ErrEnvelope es) = Left es
envelopeToEither (SuccEnvelope a) = Right a

eitherToEnvelope :: Either (OpenUnion es) a -> Envelope es a
eitherToEnvelope (Left es) = ErrEnvelope es
eitherToEnvelope (Right a) = SuccEnvelope a

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
