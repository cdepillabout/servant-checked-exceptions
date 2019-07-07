{-# LANGUAGE DataKinds #-}
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

module Servant.Checked.Exceptions.Internal.Envelope where

import Control.Applicative ((<|>))
import Control.Monad.Fix (MonadFix(mfix))
import Data.Aeson
  ( FromJSON(parseJSON)
  , ToJSON(toJSON)
  , Value
  , (.:)
  , (.=)
  , object
  , withObject
  )
import Data.Aeson.Types (Parser)
import Data.Data (Data)
import Data.Functor.Classes
  ( Show1
  , liftShowsPrec
  , showsUnaryWith
  )
import Data.Semigroup (Semigroup((<>), stimes), stimesIdempotent)
import Data.Typeable (Typeable)
import Data.WorldPeace
  ( Contains
  , ElemRemove
  , IsMember
  , OpenUnion
  , Remove
  , ReturnX
  , ToOpenProduct
  , absurdUnion
  , catchesOpenUnion
  , openUnionHandle
  , openUnionLift
  , openUnionPrism
  , openUnionRemove
  , relaxOpenUnion
  )
import GHC.Generics (Generic)

import Servant.Checked.Exceptions.Internal.Prism
       (Iso, Prism, Prism', iso, preview, prism)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> import Data.Aeson (encode)
-- >>> import Data.ByteString.Lazy.Char8 (hPutStrLn)
-- >>> import Data.Text (Text)
-- >>> import System.IO (stdout)
-- >>> import Text.Read (readMaybe)
-- >>> import Servant.Checked.Exceptions.Internal.Prism (review)
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
-- 'Servant.Checked.Exceptions.Internal.Union.Union'.
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

instance (Show (OpenUnion es)) => Show1 (Envelope es) where
  liftShowsPrec
    :: (Int -> a -> ShowS)
    -> ([a] -> ShowS)
    -> Int
    -> Envelope es a
    -> ShowS
  liftShowsPrec showA _ d (SuccEnvelope a) =
    showsUnaryWith showA "SuccEnvelope" d a
  liftShowsPrec _ _ d (ErrEnvelope es) =
    showsUnaryWith showsPrec "ErrEnvelope" d es

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
toSuccEnvelope = pure

-- | 'pureErrEnvelope' is 'toErrEnvelope' lifted up to an 'Applicative'.
--
-- >>> pureErrEnvelope 'c' :: Maybe (Envelope '[Char] Int)
-- Just (ErrEnvelope (Identity 'c'))
pureErrEnvelope :: (Applicative m, IsMember e es) => e -> m (Envelope es a)
pureErrEnvelope = pure . toErrEnvelope

-- | 'pureSuccEnvelope' is 'toSuccEnvelope' lifted up to an 'Applicative'.
--
-- >>> pureSuccEnvelope 3 :: Maybe (Envelope '[Char] Int)
-- Just (SuccEnvelope 3)
pureSuccEnvelope :: Applicative m => a -> m (Envelope es a)
pureSuccEnvelope = pure . toSuccEnvelope

-- | Case analysis for 'Envelope's.
--
-- ==== __Examples__
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

-- | Similar to 'liftA2', but more general.  This allows you to operate on two
-- 'Envelope's with different sets of errors.  The resulting 'Envelope' is a
-- combination of the errors in each of the input 'Envelope's.
--
-- ==== __Examples__
--
-- >>> let env1 = toSuccEnvelope "hello" :: Envelope '[Double, Int] String
-- >>> let env2 = toSuccEnvelope " world" :: Envelope '[Char] String
-- >>> liftA2Envelope (<>) env1 env2 :: Envelope '[Double, Int, Char] String
-- SuccEnvelope "hello world"
--
-- If either of the 'Envelope's is an 'ErrEnvelope', then return the 'ErrEnvelope'.
--
-- >>> let env3 = toErrEnvelope "some err" :: Envelope '[String, Double] Int
-- >>> let env4 = toSuccEnvelope 1 :: Envelope '[Char] Int
-- >>> liftA2Envelope (+) env3 env4 :: Envelope '[String, Double, Char] Int
-- ErrEnvelope (Identity "some err")
--
-- >>> let env5 = toSuccEnvelope "hello" :: Envelope '[Char] String
-- >>> let env6 = toErrEnvelope 3.5 :: Envelope '[(), Double] String
-- >>> liftA2Envelope (<>) env5 env6 :: Envelope '[Char, (), Double] String
-- ErrEnvelope (Identity 3.5)
--
-- If both of the 'Envelope's is an 'ErrEnvelope', then short-circuit and only
-- return the first 'ErrEnvelope'.
--
-- >>> let env7 = toErrEnvelope 3.5 :: Envelope '[(), Double] String
-- >>> let env8 = toErrEnvelope 'x' :: Envelope '[Int, Char] String
-- >>> liftA2Envelope (<>) env7 env8 :: Envelope '[(), Double, Int, Char] String
-- ErrEnvelope (Identity 3.5)
liftA2Envelope :: (Contains es1 fullEs, Contains es2 fullEs) => (a -> b -> c) -> Envelope es1 a -> Envelope es2 b -> Envelope fullEs c
liftA2Envelope f (SuccEnvelope a) (SuccEnvelope b) = SuccEnvelope (f a b)
liftA2Envelope _ (ErrEnvelope es) _ = ErrEnvelope (relaxOpenUnion es)
liftA2Envelope _ _ (ErrEnvelope es) = ErrEnvelope (relaxOpenUnion es)

-- | This is like 'liftA2Envelope' but for monadic bind ('>>=').
--
-- This allows you to bind on 'Envelope's that contain different errors.
--
-- The resulting 'Envelope' must have a superset of the errors in two input
-- 'Envelope's.
--
-- ==== __Examples__
--
-- >>> let env1 = toSuccEnvelope "hello" :: Envelope '[Double, Int] String
-- >>> let f1 str = toSuccEnvelope (length str) :: Envelope '[Char] Int
-- >>> bindEnvelope env1 f1 :: Envelope '[Double, Int, Char] Int
-- SuccEnvelope 5
--
-- If either of the 'Envelope's is an 'ErrEnvelope', then return the 'ErrEnvelope'.
--
-- >>> let env2 = toErrEnvelope "some err" :: Envelope '[String, Double] Int
-- >>> let f2 i = toSuccEnvelope (i + 1) :: Envelope '[Char] Int
-- >>> bindEnvelope env2 f2 :: Envelope '[String, Double, Char] Int
-- ErrEnvelope (Identity "some err")
--
-- >>> let env3 = toSuccEnvelope "hello" :: Envelope '[Char] String
-- >>> let f3 _ = toErrEnvelope 3.5 :: Envelope '[(), Double] Int
-- >>> bindEnvelope env3 f3 :: Envelope '[Char, (), Double] Int
-- ErrEnvelope (Identity 3.5)
--
-- If both of the 'Envelope's is an 'ErrEnvelope', then short-circuit and only
-- return the first 'ErrEnvelope'.
--
-- >>> let env4 = toErrEnvelope 3.5 :: Envelope '[(), Double] String
-- >>> let f4 _ = toErrEnvelope 'x' :: Envelope '[Int, Char] String
-- >>> bindEnvelope env4 f4 :: Envelope '[Char, (), Double, Int] String
-- ErrEnvelope (Identity 3.5)
bindEnvelope
  :: (Contains es1 fullEs, Contains es2 fullEs)
  => Envelope es1 a
  -> (a -> Envelope es2 b)
  -> Envelope fullEs b
bindEnvelope (SuccEnvelope a) f = relaxEnvelope $ f a
bindEnvelope (ErrEnvelope u) _ = relaxEnvelope (ErrEnvelope u)

-- | Unwrap an 'Envelope' that cannot contain an error.
--
-- ==== __Examples__
--
-- >>> let env = toSuccEnvelope "hello" :: Envelope '[] String
-- >>> emptyEnvelope env
-- "hello"
emptyEnvelope :: Envelope '[] a -> a
emptyEnvelope (SuccEnvelope a) = a
emptyEnvelope (ErrEnvelope es) = absurdUnion es

-- | Just like 'Data.Either.fromEither' but for 'Envelope'.
--
-- ==== __Examples__
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
-- ==== __Examples__
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
-- Most users will not use '_ErrEnvelope', but instead '_ErrEnvelopeErr'.
--
-- ==== __Examples__
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
_ErrEnvelope :: Prism (Envelope es a) (Envelope es' a) (OpenUnion es) (OpenUnion es')
_ErrEnvelope = prism ErrEnvelope $ envelope Right (Left . SuccEnvelope)

-- | Lens-compatible 'Prism' to pull out a specific @e@ from an 'ErrEnvelope'.
--
-- Most users will use '_ErrEnvelopeErr' instead of '_ErrEnvelope'.
--
-- ==== __Examples__
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
_ErrEnvelopeErr :: forall e es a. IsMember e es => Prism' (Envelope es a) e
_ErrEnvelopeErr = _ErrEnvelope . openUnionPrism

-- | Pull out a specific @e@ from an 'ErrEnvelope'.
--
-- ==== __Examples__
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

-- | An alternate case anaylsis for an 'Envelope'.  This method uses a tuple
-- containing handlers for each potential value of the 'Envelope'.  This is
-- somewhat similar to the 'Control.Exception.catches' function.
--
-- When working with an 'Envelope' with a large number of possible error types,
-- it can be easier to use 'catchesEnvelope' than 'envelope'.
--
-- ==== __Examples__
--
-- Here is an example of handling an 'SuccEnvelope' with two possible error values.
-- Notice that a normal tuple is used:
--
-- >>> let env = toSuccEnvelope 2.0 :: Envelope '[Int, String] Double
-- >>> let intHandler = (\int -> show int) :: Int -> String
-- >>> let strHandler = (\str -> str) :: String -> String
-- >>> let succHandler = (\dbl -> "got a double") :: Double -> String
-- >>> catchesEnvelope (intHandler, strHandler) succHandler env :: String
-- "got a double"
--
-- Here is an example of handling an 'ErrEnvelope' with two possible error values.
-- Notice that a normal tuple is used to hold the handlers:
--
-- >>> let env = toErrEnvelope (3 :: Int) :: Envelope '[Int, String] Double
-- >>> let intHandler = (\int -> show int) :: Int -> String
-- >>> let strHandler = (\str -> str) :: String -> String
-- >>> let succHandler = (\dbl -> "got a double") :: Double -> String
-- >>> catchesEnvelope (intHandler, strHandler) succHandler env :: String
-- "3"
--
-- Given an 'Envelope' like @'Envelope' \'['Int', 'String'] 'Double'@, the type of
-- 'catchesEnvelope' becomes the following:
--
-- @
--   'catchesEnvelope'
--     :: ('Int' -> x, 'String' -> x)
--     -> ('Double' -> x)
--     -> 'Envelope' \'['Int', 'String'] 'Double'
--     -> x
-- @
--
-- Here is an example of handling an 'ErrEnvelope' with three possible values.
-- Notice how a 3-tuple is used to hold the handlers:
--
-- >>> let env = toErrEnvelope ("hi" :: String) :: Envelope '[Int, String, Char] Double
-- >>> let intHandler = (\int -> show int) :: Int -> String
-- >>> let strHandler = (\str -> str) :: String -> String
-- >>> let chrHandler = (\chr -> [chr]) :: Char -> String
-- >>> let succHandler = (\dbl -> "got a double") :: Double -> String
-- >>> catchesEnvelope (intHandler, strHandler, chrHandler) succHandler env :: String
-- "hi"
--
-- Given an 'Envelope' like @'Envelope' \'['Int', 'String', 'Char'] 'Double'@,
-- the type of 'catchesEnvelope' becomes the following:
--
-- @
--   'catchesEnvelope'
--     :: ('Int' -> x, 'String' -> x, 'Char' -> x)
--     -> ('Double' -> x)
--     -> 'Envelope' \'['Int', 'String', 'Char'] 'Double'
--     -> x
-- @
--
-- Here is an example of handling an 'ErrEnvelope' with only one possible error value.
-- Notice that a normal handler is used (not a tuple):
--
-- >>> let env = toErrEnvelope (3 :: Int) :: Envelope '[Int] Double
-- >>> let intHandler = (\int -> show int) :: Int -> String
-- >>> let succHandler = (\dbl -> "got a double") :: Double -> String
-- >>> catchesEnvelope intHandler succHandler env :: String
-- "3"
--
-- Given an 'Envelope' like @'Envelope' \'['Int'] 'Double'@, the type of
-- 'catchesEnvelope' becomes the following:
--
-- @
--   'catchesEnvelope'
--     :: ('Int' -> x)
--     -> ('Double' -> x)
--     -> 'Envelope' \'['Int'] 'Double'
--     -> x
-- @
catchesEnvelope
  :: forall tuple es a x.
     ToOpenProduct tuple (ReturnX x es)
  => tuple -> (a -> x) -> Envelope es a -> x
catchesEnvelope _ a2x (SuccEnvelope a) = a2x a
catchesEnvelope tuple _ (ErrEnvelope u) = catchesOpenUnion tuple u

-- | Change the errors type in an 'Envelope' to a larger set.
--
-- >>> let double = 3.5 :: Double
-- >>> let env = toErrEnvelope double :: Envelope '[Double, Int] Char
-- >>> relaxEnvelope env :: Envelope '[(), Int, Double, String] Char
-- ErrEnvelope (Identity 3.5)
relaxEnvelope :: Contains es biggerEs => Envelope es a -> Envelope biggerEs a
relaxEnvelope (SuccEnvelope a) = SuccEnvelope a
relaxEnvelope (ErrEnvelope u) = ErrEnvelope (relaxOpenUnion u)

-- | This function allows you to try to remove individual error types from an
-- 'Envelope'.
--
-- This can be used to handle only certain error types in an 'Envelope',
-- instead of having to handle all of them at the same time.  This can be more
-- convenient than a function like 'catchesEnvelope'.
--
-- ==== __Examples__
--
-- Pulling out an error in an 'Envelope':
--
-- >>> let env1 = toErrEnvelope "hello" :: Envelope '[String, Double] Float
-- >>> envelopeRemove env1 :: Either (Envelope '[Double] Float) String
-- Right "hello"
--
-- Failing to pull out an error in an 'Envelope':
--
-- >>> let env2 = toErrEnvelope (3.5 :: Double) :: Envelope '[String, Double] Float
-- >>> envelopeRemove env2 :: Either (Envelope '[Double] Float) String
-- Left (ErrEnvelope (Identity 3.5))
--
-- Note that if you have an 'Envelope' with multiple errors of the same type,
-- they will all be handled at the same time:
--
-- >>> let env3 = toErrEnvelope (3.5 :: Double) :: Envelope '[String, Double, Char, Double] Float
-- >>> envelopeRemove env3 :: Either (Envelope '[String, Char] Float) Double
-- Right 3.5
--
-- 'SuccEnvelope' gets passed through as expected:
--
-- >>> let env4 = toSuccEnvelope 3.5 :: Envelope '[String, Double] Float
-- >>> envelopeRemove env4 :: Either (Envelope '[Double] Float) String
-- Left (SuccEnvelope 3.5)
envelopeRemove
  :: forall e es a
   . ElemRemove e es
  => Envelope es a
  -> Either (Envelope (Remove e es) a) e
envelopeRemove (SuccEnvelope a) = Left (SuccEnvelope a)
envelopeRemove (ErrEnvelope u) =
  case openUnionRemove u of
    Left u2 -> Left (ErrEnvelope u2)
    Right e -> Right e

-- | Handle a single case in an 'Envelope'.  This is similar to 'envelope'
-- but lets you handle any case within the 'Envelope', not just the first one.
--
-- ==== __Examples__
--
-- Handling the first item in an 'Envelope':
--
-- >>> let env1 = toErrEnvelope 3.5 :: Envelope '[Double, Int] Char
-- >>> let printDouble = print :: Double -> IO ()
-- >>> let printEnv = print :: Envelope '[Int] Char -> IO ()
-- >>> envelopeHandle printEnv printDouble env1
-- 3.5
--
-- Handling a middle item in an 'Envelope':
--
-- >>> let env2 = toErrEnvelope (3.5 :: Double) :: Envelope '[Char, Double, Int] Float
-- >>> let printEnv = print :: Envelope '[Char, Int] Float -> IO ()
-- >>> envelopeHandle printEnv printDouble env2
-- 3.5
--
-- Failing to handle an item in an 'Envelope'.  In the following example, the
-- @printEnv@ function is called:
--
-- >>> let env3 = toErrEnvelope 'c' :: Envelope '[Char, Double, Int] Float
-- >>> let printEnv = print :: Envelope '[Char, Int] Float -> IO ()
-- >>> envelopeHandle printEnv printDouble env3
-- ErrEnvelope (Identity 'c')
--
-- If you have duplicates in your 'Envelope', they will both get handled with
-- a single call to 'unionHandle'.
--
-- >>> let env4 = toErrEnvelope 3.5 :: Envelope '[Double, Double, Int] Char
-- >>> let printEnv = print :: Envelope '[Int] Char -> IO ()
-- >>> envelopeHandle printEnv printDouble env4
-- 3.5
--
-- 'SuccEnvelope' gets passed through as expected:
--
-- >>> let env5 = toSuccEnvelope 3.5 :: Envelope '[String, Double] Float
-- >>> let printEnv = print :: Envelope '[String] Float -> IO ()
-- >>> envelopeHandle printEnv printDouble env5
-- SuccEnvelope 3.5
envelopeHandle
  :: ElemRemove e es
  => (Envelope (Remove e es) a -> x)
  -> (e -> x)
  -> Envelope es a
  -> x
envelopeHandle handler _ (SuccEnvelope a) = handler (SuccEnvelope a)
envelopeHandle handler errHandler (ErrEnvelope u) =
  openUnionHandle (handler . ErrEnvelope) errHandler u
