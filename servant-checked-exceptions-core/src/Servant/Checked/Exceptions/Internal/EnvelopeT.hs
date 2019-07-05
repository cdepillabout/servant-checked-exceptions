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
Module      :  Servant.Checked.Exceptions.Internal.EnvelopeT

Copyright   :  Dennis Gosnell 2019
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module defines the 'EnvelopeT' type and helper functions. 'EnvelopeT' is a
short-circuiting monad transformer.

'Envelope' is similar to 'Either' where multiple errors types are possible.
'EnvelopeT' is similar to 'ExceptT' in a similar manner.
-}

module Servant.Checked.Exceptions.Internal.EnvelopeT where

import Control.Monad.Except (ExceptT(ExceptT))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.WorldPeace
  ( IsMember
  , OpenUnion
  , ReturnX
  , ToOpenProduct
  , absurdUnion
  , catchesOpenUnion
  , openUnionLift
  , openUnionPrism
  )

import Servant.Checked.Exceptions.Internal.Envelope
  ( Envelope(ErrEnvelope, SuccEnvelope)
  , eitherToEnvelope
  , emptyEnvelope
  , envelopeToEither
  , pureErrEnvelope
  , pureSuccEnvelope
  )

data EnvelopeT es m a = EnvelopeT
  { runEnvelopeT :: m (Envelope es a)
  } deriving Functor

instance Monad m => Applicative (EnvelopeT es m) where
  pure :: a -> EnvelopeT es m a
  pure a = EnvelopeT $ pureSuccEnvelope a

  (<*>) :: EnvelopeT es m (a -> b) -> EnvelopeT es m a -> EnvelopeT es m b
  EnvelopeT a2b <*> EnvelopeT a = EnvelopeT $ go <$> a2b <*> a
    where
      go :: Envelope es (a -> b) -> Envelope es a -> Envelope es b
      go = (<*>)

instance Monad m => Monad (EnvelopeT es m) where
  (>>=) :: EnvelopeT es m a -> (a -> EnvelopeT es m b) -> EnvelopeT es m b
  (EnvelopeT m) >>= k = EnvelopeT $ do
    env <- m
    case env of
      SuccEnvelope a -> runEnvelopeT $ k a
      ErrEnvelope err -> pure $ ErrEnvelope err

instance MonadTrans (EnvelopeT es) where
  lift :: Monad m => m a -> EnvelopeT es m a
  lift m = EnvelopeT $ do
    val <- m
    pureSuccEnvelope val

instance MonadIO m => MonadIO (EnvelopeT es m) where
  liftIO :: IO a -> EnvelopeT es m a
  liftIO = lift . liftIO

-- | This is 'pure' for 'EnvelopeT'.
--
-- >>> pureSuccEnvT "hello" :: EnvelopeT '[] Identity String
-- EnvelopeT (Identity (SuccEnvelope "hello"))
pureSuccEnvT :: Applicative m => a -> EnvelopeT es m a
pureSuccEnvT = EnvelopeT . pureSuccEnvelope

-- | Throw an error in an 'ErrEnvelope'.
--
-- >>> let double = 3.5 :: Double
-- >>> throwErrEnvT double :: EnvelopeT '[String, Double, Int] Identity ()
-- EnvelopeT (Identity (ErrEnvelope (Identity 3.5)))
--
-- This is similar to 'throwError', but is specialized so you can throw just
-- one of the error types.
throwErrEnvT :: (Applicative m, IsMember e es) => e -> EnvelopeT es m a
throwErrEnvT = EnvelopeT . pureErrEnvelope

-- | Convert an 'EnvelopeT' to an 'ExceptT'.
envTToExceptT :: Functor m => EnvelopeT es m a -> ExceptT (OpenUnion es) m a
envTToExceptT (EnvelopeT m) = ExceptT $ fmap envelopeToEither m

-- | Convert an 'ExceptT' to an 'EnvelopeT'.
exceptTToEnvT :: Functor m => ExceptT (OpenUnion es) m a -> EnvelopeT es m a
exceptTToEnvT (ExceptT m) = EnvelopeT $ fmap eitherToEnvelope m

-- | Safely unwrap an 'EnvelopeT'.
--
-- >>> let myenvT = pure "hello" :: EnvelopeT '[] Identity String
-- >>> emptyEnvT myenvT :: Identity String
-- Identity "hello"
emptyEnvT :: Functor m => EnvelopeT '[] m a -> m a
emptyEnvT (EnvelopeT m) = fmap emptyEnvelope m
