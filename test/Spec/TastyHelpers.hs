{-# LANGUAGE ScopedTypeVariables #-}

module Spec.TastyHelpers where

import Control.Exception (Exception, SomeException, catch)
import Data.Typeable (Typeable)
import Test.Tasty.HUnit (assertFailure)

type Selector e = e -> Bool

anyException :: Selector SomeException
anyException _ = True

-- | Extra HUnit assertion to make sure an expression throws an exception.
assertThrows
  :: forall e a.
     (Exception e, Typeable e)
  => IO a -> Selector e -> IO ()
assertThrows ioAction selector = do
  didCatch <- catch (ioAction *> pure False) (pure . selector)
  case didCatch of
    False ->
      assertFailure "expecting an exception, but no exception occurred"
    True -> pure ()

-- | Infix version of 'assertThrows'.
(@!)
  :: (Exception e, Typeable e)
  => IO a -> Selector e -> IO ()
(@!) = assertThrows

infix 1 @!
