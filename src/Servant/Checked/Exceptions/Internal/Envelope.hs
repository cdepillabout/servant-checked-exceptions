{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      :  Servant.Checked.Exceptions.Internal.Envelope

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module defines the 'Envelope' type as a wrapper around a success value, or
a set of possible errors.  The errors are an 'OpenUnion', which is an
extensible sumtype.

Other than the 'Envelope' type, the most important thing in this module is the
'ToJSON' instance for 'Envelope'.
-}

module Servant.Checked.Exceptions.Internal.Envelope
  (
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

import Servant.Checked.Exceptions.Internal.Product (ToOpenProduct)
import Servant.Checked.Exceptions.Internal.Union
       (IsMember, OpenUnion, catchesOpenUnion, openUnionLift,
        openUnionPrism)
import Servant.Checked.Exceptions.Internal.Util (ReturnX)


-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> import Control.Lens (preview, review)
-- >>> import Data.Aeson (encode)
-- >>> import Data.ByteString.Lazy.Char8 (hPutStrLn)
-- >>> import Data.Text (Text)
-- >>> import System.IO (stdout)
-- >>> import Text.Read (readMaybe)
-- >>> let putByteStrLn = hPutStrLn stdout


-- | This 'Envelope' type is a used as a wrapper around either an 'OpenUnion'
-- with an error or a successful value.  It is similar to an @'Either' e a@,
-- but where the @e@ is specialized to @'OpenUnion' es@.  The most important
-- difference from 'Either' is the the 'FromJSON' and 'ToJSON' instances.
--
-- Given an @'Envelope' \'['String', 'Double'] ()@, we know that the envelope
-- could be a 'SuccEnvelope' and contain @()@.  Or it could be a 'ErrEnvelope'
-- that contains /either/ a 'String' /or/ a 'Double'.  It might be simpler to
-- think of it as a type like @'Either' 'String' ('Either' 'Double' ())@.
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

-- | Just like 'Data.Either.fromEither' but for 'Envelope'.
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
-- Use '_SuccEnvelope' to try to destruct an 'Envelope' into an @a@:
--
-- >>> let env = toSuccEnvelope "hello" :: Envelope '[Double] String
-- >>> preview _SuccEnvelope env :: Maybe String
-- Just "hello"
--
-- Use '_SuccEnvelope' to try to destruct a 'Envelope into an @a@
-- (unsuccessfully):
--
-- >>> let double = 3.5 :: Double
-- >>> let env' = toErrEnvelope double :: Envelope '[Double] String
-- >>> preview _SuccEnvelope env' :: Maybe String
-- Nothing
_SuccEnvelope :: Prism (Envelope es a) (Envelope es b) a b
_SuccEnvelope = prism SuccEnvelope $ envelope (Left . ErrEnvelope) Right

-- | Lens-compatible 'Prism' to pull out an @'OpenUnion' es@ from a
-- 'ErrEnvelope'.
--
-- Use '_ErrEnvelope' to construct an 'Envelope':
--
-- >>> let string = "hello" :: String
-- >>> review _ErrEnvelope (openUnionLift string) :: Envelope '[String] Double
-- ErrEnvelope (Identity "hello")
--
-- Use '_ErrEnvelope' to try to destruct an 'Envelope' into an
-- @'OpenUnion' es@:
--
-- >>> let double = 3.5 :: Double
-- >>> let env = toErrEnvelope double :: Envelope '[Double] ()
-- >>> preview _ErrEnvelope env :: Maybe (OpenUnion '[Double])
-- Just (Identity 3.5)
--
-- Use '_ErrEnvelope' to try to destruct a 'Envelope into an
-- @'OpenUnion' es@ (unsuccessfully):
--
-- >>> let env' = toSuccEnvelope () :: Envelope '[Double] ()
-- >>> preview _ErrEnvelope env' :: Maybe (OpenUnion '[Double])
-- Nothing
--
-- Most users will not use '_ErrEnvelope', but instead '_ErrEnvelopeErr'.
_ErrEnvelope :: Prism (Envelope es a) (Envelope es' a) (OpenUnion es) (OpenUnion es')
_ErrEnvelope = prism ErrEnvelope $ envelope Right (Left . SuccEnvelope)

-- | Lens-compatible 'Prism' to pull out a specific @e@ from an 'ErrEnvelope'.
--
-- Use '_ErrEnvelopeErr' to construct an 'Envelope':
--
-- >>> let string = "hello" :: String
-- >>> review _ErrEnvelopeErr string :: Envelope '[String] Double
-- ErrEnvelope (Identity "hello")
--
-- Use '_ErrEnvelopeErr' to try to destruct an 'Envelope' into an @e@:
--
-- >>> let double = 3.5 :: Double
-- >>> let env = toErrEnvelope double :: Envelope '[Double] ()
-- >>> preview _ErrEnvelopeErr env :: Maybe Double
-- Just 3.5
--
-- Use '_ErrEnvelopeErr' to try to destruct a 'Envelope into an
-- @e@ (unsuccessfully):
--
-- >>> let env' = toSuccEnvelope () :: Envelope '[Double] ()
-- >>> preview _ErrEnvelopeErr env' :: Maybe Double
-- Nothing
-- >>> let env'' = toErrEnvelope 'c' :: Envelope '[Double, Char] ()
-- >>> preview _ErrEnvelopeErr env'' :: Maybe Double
-- Nothing
--
-- Most users will use '_ErrEnvelopeErr' instead of '_ErrEnvelope'.
_ErrEnvelopeErr :: forall e es a. IsMember e es => Prism' (Envelope es a) e
_ErrEnvelopeErr = _ErrEnvelope . openUnionPrism

-- | Pull out a specific @e@ from an 'ErrEnvelope'.
--
-- Successfully pull out an @e@:
--
-- >>> let double = 3.5 :: Double
-- >>> let env = toErrEnvelope double :: Envelope '[Double] ()
-- >>> errEnvelopeMatch env :: Maybe Double
-- Just 3.5
--
-- Unsuccessfully pull out an @e@:
--
-- >>> let env' = toSuccEnvelope () :: Envelope '[Double] ()
-- >>> errEnvelopeMatch env' :: Maybe Double
-- Nothing
-- >>> let env'' = toErrEnvelope 'c' :: Envelope '[Double, Char] ()
-- >>> errEnvelopeMatch env'' :: Maybe Double
-- Nothing
errEnvelopeMatch
  :: forall e es a.
     IsMember e es
  => Envelope es a -> Maybe e
errEnvelopeMatch = preview _ErrEnvelopeErr

catchesEnvelope
  :: forall tuple es a x.
     ToOpenProduct tuple (ReturnX x es)
  => tuple -> (a -> x) -> Envelope es a -> x
catchesEnvelope _ a2x (SuccEnvelope a) = a2x a
catchesEnvelope tuple _ (ErrEnvelope u) = catchesOpenUnion tuple u

-- data EnvelopeHandler es x = forall e. IsMember e es => EnvelopeHandler (e -> x)

-- | This 'ToJSON' instance encodes an 'Envelope' as an object with one of two
-- keys depending on whether it is a 'SuccEnvelope' or an 'ErrEnvelope'.
--
-- Here is an example of a 'SuccEnvelope':
--
-- >>> let string = "hello" :: String
-- >>> let env = toSuccEnvelope string :: Envelope '[Double] String
-- >>> putByteStrLn $ encode env
-- {"data":"hello"}
--
-- Here is an example of a 'ErrEnvelope':
--
-- >>> let double = 3.5 :: Double
-- >>> let env' = toErrEnvelope double :: Envelope '[Double] String
-- >>> putByteStrLn $ encode env'
-- {"err":3.5}
instance (ToJSON (OpenUnion es), ToJSON a) => ToJSON (Envelope es a) where
  toJSON :: Envelope es a -> Value
  toJSON (ErrEnvelope es) = object ["err" .= es]
  toJSON (SuccEnvelope a) = object ["data" .= a]

-- | This is only a valid instance when the 'FromJSON' instances for the @es@
-- don't overlap.
--
-- For an explanation, see the documentation on the 'FromJSON' instance for
-- 'Union'.
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
