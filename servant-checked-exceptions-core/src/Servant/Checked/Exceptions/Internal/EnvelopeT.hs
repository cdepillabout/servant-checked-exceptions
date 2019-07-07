{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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

import Control.Monad.Except (ExceptT(ExceptT), MonadError(throwError, catchError))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ask, local, reader)
import Control.Monad.RWS (MonadRWS)
import Control.Monad.State (MonadState, get, put, state)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Writer (MonadWriter, listen, pass, tell, writer)
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
  , ElemRemove
  , IsMember
  , OpenUnion
  , Remove
  , ReturnX
  , ToOpenProduct
  , relaxOpenUnion
  )

import Servant.Checked.Exceptions.Internal.Envelope
  ( Envelope(ErrEnvelope, SuccEnvelope)
  , catchesEnvelope
  , liftA2Envelope
  , eitherToEnvelope
  , emptyEnvelope
  , envelope
  , envelopeRemove
  , envelopeToEither
  , errEnvelopeMatch
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

instance MonadRWS r w s m => MonadRWS r w s (EnvelopeT es m)

instance MonadError error m => MonadError error (EnvelopeT es m) where
  throwError = EnvelopeT . throwError

  catchError
    :: forall a
     . EnvelopeT es m a
    -> (error -> EnvelopeT es m a)
    -> EnvelopeT es m a
  catchError (EnvelopeT m) handler = EnvelopeT $ catchError m innerRunner
    where
      innerRunner :: error -> m (Envelope es a)
      innerRunner = runEnvelopeT . handler

instance MonadReader r m => MonadReader r (EnvelopeT es m) where
  ask = lift ask
  local f (EnvelopeT m) = EnvelopeT (local f m)
  reader = lift . reader

instance MonadState s m => MonadState s (EnvelopeT es m) where
  get = lift get
  put = lift . put
  state = lift . state

instance MonadWriter w m => MonadWriter w (EnvelopeT es m) where
  writer = lift . writer
  tell = lift . tell
  listen (EnvelopeT m) =
    EnvelopeT $ do
      (envelopeA, w) <- listen m
      pure $ fmap (,w) envelopeA
  pass (EnvelopeT m) =
    EnvelopeT $ do
      envel <- m
      pass . pure $
        case envel of
          SuccEnvelope (a, f) -> (SuccEnvelope a, f)
          ErrEnvelope es -> (ErrEnvelope es, id)

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

-- | Case analysis for 'EnvelopeT'.
--
-- ==== __Examples__
--
--  Here is an example of matching on a 'SuccEnvelope':
--
-- >>> let env = pure "hello" :: EnvelopeT '[Double, Int] Identity String
-- >>> envelopeT (\_ -> Identity "not a String") Identity env
-- Identity "hello"
--
-- Here is an example of matching on a 'ErrEnvelope':
--
-- >>> let double = 3.5 :: Double
-- >>> let env' = throwErrEnvT double :: EnvelopeT '[Double, Int] Identity String
-- >>> envelopeT (\_ -> Identity "not a String") Identity env'
-- Identity "not a String"
envelopeT :: Monad m => (OpenUnion es -> m c) -> (a -> m c) -> EnvelopeT es m a -> m c
envelopeT errHandler succHandler (EnvelopeT m) = do
  envel <- m
  envelope errHandler succHandler envel

-- | Slight simplification of 'envelopeT'.
--
-- ==== __Examples__
--
--  Here is an example of successfully matching:
--
-- >>> let env = pure "hello" :: EnvelopeT '[Double, Int] Identity String
-- >>> fromEnvT (\_ -> Identity "not a String") env
-- Identity "hello"
--
-- Here is an example of unsuccessfully matching:
--
-- >>> let double = 3.5 :: Double
-- >>> let env' = throwErrEnvT double :: EnvelopeT '[Double, Int] Identity String
-- >>> fromEnvT (\_ -> Identity "not a String") env'
-- Identity "not a String"
fromEnvT :: Monad m => (OpenUnion es -> m a) -> EnvelopeT es m a -> m a
fromEnvT f = envelopeT f pure

-- | Flipped version of 'fromEnvT'.
fromEnvTOr :: Monad m => EnvelopeT es m a -> (OpenUnion es -> m a) -> m a
fromEnvTOr = flip fromEnvT

-- | Try to pull out a specific @e@ from an 'ErrEnvelope'.
--
-- ==== __Examples__
--
-- Successfully pull out an @e@:
--
-- >>> let double = 3.5 :: Double
-- >>> let env = throwErrEnvT double :: EnvelopeT '[Double, Char] Identity ()
-- >>> errEnvTMatch env :: Identity (Maybe Double)
-- Identity (Just 3.5)
--
-- Unsuccessfully pull out an @e@:
--
-- >>> let env' = pure () :: EnvelopeT '[String, Double] Identity ()
-- >>> errEnvTMatch env' :: Identity (Maybe Double)
-- Identity Nothing
errEnvTMatch
  :: forall e es m a.
     (Functor m, IsMember e es)
  => EnvelopeT es m a
  -> m (Maybe e)
errEnvTMatch (EnvelopeT m) = fmap errEnvelopeMatch m

-- | An alternate case anaylsis for an 'EnvelopeT'.  This method uses a tuple
-- containing handlers for each potential value of the underlying 'Envelope'.
-- This is somewhat similar to the 'Control.Exception.catches' function.
--
-- When working with an 'Envelope' with a large number of possible error types,
-- it can be easier to use 'catchesEnvT' than 'envelopeT'.
--
-- ==== __Examples__
--
-- Here is an example of handling an 'SuccEnvelope' with two possible error values.
-- Notice that a normal tuple is used:
--
-- >>> let env = pure 2.0 :: EnvelopeT '[Int, String] IO Double
-- >>> let intHandler = (\int -> pure $ show int) :: Int -> IO String
-- >>> let strHandler = (\str -> pure str) :: String -> IO String
-- >>> let succHandler = (\dbl -> pure "got a double") :: Double -> IO String
-- >>> catchesEnvT (intHandler, strHandler) succHandler env :: IO String
-- "got a double"
--
-- Here is an example of handling an 'ErrEnvelope' with two possible error values.
-- Notice that a normal tuple is used to hold the handlers:
--
-- >>> let env = throwErrEnvT (3 :: Int) :: EnvelopeT '[Int, String] Identity Double
-- >>> let intHandler = (\int -> Identity $ show int) :: Int -> Identity String
-- >>> let strHandler = (\str -> Identity str) :: String -> Identity String
-- >>> let succHandler = (\dbl -> Identity "got a double") :: Double -> Identity String
-- >>> catchesEnvT (intHandler, strHandler) succHandler env :: Identity String
-- Identity "3"
--
-- Given an 'EnvelopeT' like @'EnvelopeT' \'['Int', 'String'] 'IO' 'Double'@,
-- the type of 'catchesEnvT' becomes the following:
--
-- @
--   'catchesEnvT'
--     :: ('Int' -> 'IO' x, 'String' -> 'IO' x)
--     -> ('Double' -> 'IO' x)
--     -> 'EnvelopeT' \'['Int', 'String'] 'IO' 'Double'
--     -> 'IO' x
-- @
--
-- Here is an example of handling an 'ErrEnvelope' with three possible values.
-- Notice how a 3-tuple is used to hold the handlers:
--
-- >>> let env = throwErrEnvT ("hi" :: String) :: EnvelopeT '[Int, String, Char] IO Double
-- >>> let intHandler = (\int -> pure $ show int) :: Int -> IO String
-- >>> let strHandler = (\str -> pure str) :: String -> IO String
-- >>> let chrHandler = (\chr -> pure [chr]) :: Char -> IO String
-- >>> let succHandler = (\dbl -> pure "got a double") :: Double -> IO String
-- >>> catchesEnvT (intHandler, strHandler, chrHandler) succHandler env :: IO String
-- "hi"
--
-- Given an 'Envelope' like @'EnvelopeT' \'['Int', 'String', 'Char'] 'IO' 'Double'@,
-- the type of 'catchesEnvT' becomes the following:
--
-- @
--   'catchesEnvT'
--     :: ('Int' -> 'IO' x, 'String' -> 'IO' x, 'Char' -> 'IO' x)
--     -> ('Double' -> 'IO' x)
--     -> 'EnvelopeT' \'['Int', 'String', 'Char'] 'IO' 'Double'
--     -> x
-- @
catchesEnvT
  :: forall tuple es m a x
   .  (Monad m, ToOpenProduct tuple (ReturnX (m x) es))
  => tuple -> (a -> m x) -> EnvelopeT es m a -> m x
catchesEnvT tuple a2mx (EnvelopeT m) = do
  envel <- m
  catchesEnvelope tuple a2mx envel

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
-- ==== __Examples__
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
-- from both 'EnvelopeT's. Similar to 'liftA2' but more general.
--
-- ==== __Examples__
--
-- >>> let env1 = pure "hello" :: EnvelopeT '[Double, Int] Identity String
-- >>> let env2 = pure " world" :: EnvelopeT '[Char]  Identity String
-- >>> liftA2EnvT (<>) env1 env2 :: EnvelopeT '[Double, Int, Char] Identity String
-- EnvelopeT (Identity (SuccEnvelope "hello world"))
--
-- If either of the 'Envelope's is an 'ErrEnvelope', then return the 'ErrEnvelope'.
--
-- >>> let env3 = throwErrEnvT "some err" :: EnvelopeT '[String, Double] Identity Int
-- >>> let env4 = pure 1 :: EnvelopeT '[Char]  Identity Int
-- >>> liftA2EnvT (+) env3 env4 :: EnvelopeT '[String, Double, Char] Identity Int
-- EnvelopeT (Identity (ErrEnvelope (Identity "some err")))
--
-- >>> let env5 = pure "hello" :: EnvelopeT '[Char] Identity String
-- >>> let env6 = throwErrEnvT 3.5 :: EnvelopeT '[(), Double] Identity String
-- >>> liftA2EnvT (<>) env5 env6 :: EnvelopeT '[Char, (), Double] Identity String
-- EnvelopeT (Identity (ErrEnvelope (Identity 3.5)))
--
-- If both of the 'EnvelopeT's is an 'ErrEnvelope', then short-circuit and only
-- return the first 'ErrEnvelope'.
--
-- >>> let env7 = throwErrEnvT 4.5 :: EnvelopeT '[(), Double] Identity String
-- >>> let env8 = throwErrEnvT 'x' :: EnvelopeT '[Int, Char] Identity String
-- >>> liftA2EnvT (<>) env7 env8 :: EnvelopeT '[(), Double, Int, Char] Identity String
-- EnvelopeT (Identity (ErrEnvelope (Identity 4.5)))
liftA2EnvT
  :: (Contains es1 fullEs, Contains es2 fullEs, Applicative m)
  => (a -> b -> c)
  -> EnvelopeT es1 m a
  -> EnvelopeT es2 m b
  -> EnvelopeT fullEs m c
liftA2EnvT f (EnvelopeT m) (EnvelopeT n) =
  EnvelopeT $ liftA2Envelope f <$> m <*> n

-- | This is like 'liftA2EnvT' but for monadic bind ('>>=').
--
-- This allows you to bind on 'EnvelopeT's that contain different errors.
--
-- The resulting 'EnvelopeT' must have a superset of the errors in two input
-- 'EnvelopeT's.
--
-- ==== __Examples__
--
-- >>> let env1 = pure "hello" :: EnvelopeT '[Double, Int] Identity String
-- >>> let f1 str = pure (length str) :: EnvelopeT '[Char] Identity Int
-- >>> bindEnvT env1 f1 :: EnvelopeT '[Double, Int, Char] Identity Int
-- EnvelopeT (Identity (SuccEnvelope 5))
--
-- If either of the 'EnvelopeT's holds an 'ErrEnvelope', then return the 'ErrEnvelope'.
--
-- >>> let env2 = throwErrEnvT "some err" :: EnvelopeT '[String, Double] Identity Int
-- >>> let f2 i = pureSuccEnvT (i + 1) :: EnvelopeT '[Char] Identity Int
-- >>> bindEnvT env2 f2 :: EnvelopeT '[String, Double, Char] Identity Int
-- EnvelopeT (Identity (ErrEnvelope (Identity "some err")))
--
-- >>> let env3 = pureSuccEnvT "hello" :: EnvelopeT '[Char] Identity String
-- >>> let f3 _ = throwErrEnvT 3.5 :: EnvelopeT '[(), Double] Identity Int
-- >>> bindEnvT env3 f3 :: EnvelopeT '[Char, (), Double] Identity Int
-- EnvelopeT (Identity (ErrEnvelope (Identity 3.5)))
--
-- If both of the 'Envelope's is an 'ErrEnvelope', then short-circuit and only
-- return the first 'ErrEnvelope'.
--
-- >>> let env4 = throwErrEnvT 3.5 :: EnvelopeT '[(), Double] Maybe String
-- >>> let f4 _ = throwErrEnvT 'x' :: EnvelopeT '[Int, Char] Maybe String
-- >>> bindEnvT env4 f4 :: EnvelopeT '[Char, (), Double, Int] Maybe String
-- EnvelopeT (Just (ErrEnvelope (Identity 3.5)))
bindEnvT
  :: (Contains es1 fullEs, Contains es2 fullEs, Monad m)
  => EnvelopeT es1 m a
  -> (a -> EnvelopeT es2 m b)
  -> EnvelopeT fullEs m b
bindEnvT (EnvelopeT m) f =
  EnvelopeT $ do
    envel1 <- m
    case envel1 of
      SuccEnvelope a ->
        let x = f a
        in runEnvelopeT $ relaxEnvT x
      ErrEnvelope u ->
        let fullEs = relaxOpenUnion u
        in pure $ ErrEnvelope fullEs

-- TODO: Documentation
envTRemove
  :: forall e es m a
   . (ElemRemove e es, Functor m)
  => EnvelopeT es m a
  -> EnvelopeT (Remove e es) m (Either a e)
envTRemove (EnvelopeT m) = EnvelopeT $ fmap go m
  where
  go :: Envelope es a -> Envelope (Remove e es) (Either a e)
  go envel =
    case envelopeRemove envel of
      Right e -> SuccEnvelope (Right e)
      Left envel -> fmap Left envel

envTHandle
  :: (ElemRemove e es, Monad m)
  => (a -> EnvelopeT (Remove e es) m x)
  -> (e -> EnvelopeT (Remove e es) m x)
  -> EnvelopeT es m a
  -> EnvelopeT (Remove e es) m x
envTHandle aHandler eHandler envT = do
  aOrE <- envTRemove envT
  either aHandler eHandler aOrE
