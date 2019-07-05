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
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Functor.Classes
  ( Show1
  , liftShowList
  , liftShowsPrec
  , showsPrec1
  , showsUnaryWith
  )
import Data.Functor.Contravariant (Contravariant(contramap))
import Data.WorldPeace
  ( Contains
  , IsMember
  , OpenUnion
  )

import Servant.Checked.Exceptions.Internal.Envelope
  ( Envelope(ErrEnvelope, SuccEnvelope)
  , combineEnvelopes
  , eitherToEnvelope
  , emptyEnvelope
  , envelope
  , envelopeToEither
  , pureErrEnvelope
  , pureSuccEnvelope
  , relaxEnvelope
  )

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> import Data.Functor.Identity (Identity(Identity))

data EnvelopeT es m a = EnvelopeT
  { runEnvelopeT :: m (Envelope es a)
  } deriving Functor

instance (Show (OpenUnion es), Show1 m) => Show1 (EnvelopeT es m) where
  liftShowsPrec
    :: forall a
     . (Int -> a -> ShowS)
    -> ([a] -> ShowS)
    -> Int
    -> EnvelopeT es m a -> ShowS
  liftShowsPrec sp sl d (EnvelopeT m) =
    showsUnaryWith showInnerM "EnvelopeT" d m
    where
      showInnerM :: Int -> m (Envelope es a) -> ShowS
      showInnerM = liftShowsPrec sp' sl'

      sp' :: Int -> Envelope es a -> ShowS
      sp' = liftShowsPrec sp sl

      sl' :: [Envelope es a] -> ShowS
      sl' = liftShowList sp sl

instance (Show (OpenUnion e), Show1 m, Show a) => Show (EnvelopeT e m a) where
  showsPrec = showsPrec1

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

instance Foldable m => Foldable (EnvelopeT es m) where
  foldMap f (EnvelopeT m) = foldMap (envelope (const mempty) f) m

instance (Traversable m) => Traversable (EnvelopeT es m) where
  traverse
    :: Applicative f => (a -> f b) -> EnvelopeT es m a -> f (EnvelopeT es m b)
  traverse f (EnvelopeT m) =
    fmap EnvelopeT $
      traverse (envelope (pure . ErrEnvelope) (fmap SuccEnvelope . f)) m

instance Contravariant m => Contravariant (EnvelopeT es m) where
  contramap :: (b -> a) -> EnvelopeT es m a -> EnvelopeT es m b
  contramap f (EnvelopeT m) = EnvelopeT $ contramap (fmap f) m

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
-- >>> let myenvT = pure "hello" :: EnvelopeT '[] IO String
-- >>> emptyEnvT myenvT :: IO String
-- "hello"
emptyEnvT :: Functor m => EnvelopeT '[] m a -> m a
emptyEnvT (EnvelopeT m) = fmap emptyEnvelope m

-- | Change the errors type in an 'EnvelopeT' to a larger set.
--
-- >>> let double = 3.5 :: Double
-- >>> let envT1 = throwErrEnvT double :: EnvelopeT '[Int, Double] Identity Float
-- >>> relaxEnvT envT1 :: EnvelopeT '[Char, Int, String, Double] Identity Float
-- EnvelopeT (Identity (ErrEnvelope (Identity 3.5)))
--
-- >>> let envT2 = pure double :: EnvelopeT '[Char, Int] Identity Double
-- >>> relaxEnvT envT2 :: EnvelopeT '[(), Char, String, Int] Identity Double
-- EnvelopeT (Identity (SuccEnvelope 3.5))
relaxEnvT :: (Functor m, Contains es1 es2) => EnvelopeT es1 m a -> EnvelopeT es2 m a
relaxEnvT (EnvelopeT m) = EnvelopeT $ fmap relaxEnvelope m

-- | Combine two 'EnvelopeT's.  Generalize the set of errors to include the errors
-- from both 'EnvelopeT's.
--
-- >>> let env1 = pure "hello" :: EnvelopeT '[Double, Int] Identity String
-- >>> let env2 = pure " world" :: EnvelopeT '[Char]  Identity String
-- >>> combineEnvT (<>) env1 env2 :: EnvelopeT '[Double, Int, Char] Identity String
-- EnvelopeT (Identity (SuccEnvelope "hello world"))
--
-- If either of the 'Envelope's is an 'ErrEnvelope', then return the 'ErrEnvelope'.
--
-- >>> let env3 = throwErrEnvT "some err" :: EnvelopeT '[String, Double] Identity Int
-- >>> let env4 = pure 1 :: EnvelopeT '[Char]  Identity Int
-- >>> combineEnvT (+) env3 env4 :: EnvelopeT '[String, Double, Char] Identity Int
-- EnvelopeT (Identity (ErrEnvelope (Identity "some err")))
--
-- >>> let env5 = pure "hello" :: EnvelopeT '[Char] Identity String
-- >>> let env6 = throwErrEnvT 3.5 :: EnvelopeT '[(), Double] Identity String
-- >>> combineEnvT (<>) env5 env6 :: EnvelopeT '[Char, (), Double] Identity String
-- EnvelopeT (Identity (ErrEnvelope (Identity 3.5)))
--
-- If both of the 'EnvelopeT's is an 'ErrEnvelope', then short-circuit and only
-- return the first 'ErrEnvelope'.
--
-- >>> let env7 = throwErrEnvT 4.5 :: EnvelopeT '[(), Double] Identity String
-- >>> let env8 = throwErrEnvT 'x' :: EnvelopeT '[Int, Char] Identity String
-- >>> combineEnvT (<>) env7 env8 :: EnvelopeT '[(), Double, Int, Char] Identity String
-- EnvelopeT (Identity (ErrEnvelope (Identity 4.5)))
combineEnvT
  :: (Contains es1 fullEs, Contains es2 fullEs, Applicative m)
  => (a -> b -> c)
  -> EnvelopeT es1 m a
  -> EnvelopeT es2 m b
  -> EnvelopeT fullEs m c
combineEnvT f (EnvelopeT m) (EnvelopeT n) = EnvelopeT $ combineEnvelopes f <$> m <*> n
