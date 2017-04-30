{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      :  Servant.Checked.Exceptions.Internal.Product

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module defines an open product type.  This is used in the case-analysis
handler for the open sum type.
-}

module Servant.Checked.Exceptions.Internal.Product
  where

import Data.Functor.Identity (Identity(Identity))

-- $setup
-- >>> -- :set -XDataKinds

-------------
-- Product --
-------------

-- | An extensible product type.  This is similar to
-- 'Servant.Checked.Exceptions.Internal.Union.Union', except a product type
-- instead of a sum type.
data Product (f :: u -> *) (as :: [u]) where
  Nil :: Product f '[]
  Cons :: !(f a) -> Product f as -> Product f (a ': as)

-- | This type class provides a way to turn a tuple into a 'Product'.
class ToProduct (tuple :: *) (f :: u -> *) (as :: [u]) | f as -> tuple where
  toProduct :: tuple -> Product f as

instance forall (f :: u -> *) (a :: u). ToProduct (f a) f '[a] where
  toProduct :: f a -> Product f '[a]
  toProduct fa = Cons fa Nil

instance forall (f :: u -> *) (a :: u) (b :: u). ToProduct (f a, f b) f '[a, b] where
  toProduct :: (f a, f b) -> Product f '[a, b]
  toProduct (fa, fb) = Cons fa $ Cons fb Nil

instance forall (f :: u -> *) (a :: u) (b :: u) (c :: u). ToProduct (f a, f b, f c) f '[a, b, c] where
  toProduct :: (f a, f b, f c) -> Product f '[a, b, c]
  toProduct (fa, fb, fc) = Cons fa $ Cons fb $ Cons fc Nil

instance forall (f :: u -> *) (a :: u) (b :: u) (c :: u) (d :: u). ToProduct (f a, f b, f c, f d) f '[a, b, c, d] where
  toProduct :: (f a, f b, f c, f d) -> Product f '[a, b, c, d]
  toProduct (fa, fb, fc, fd) = Cons fa $ Cons fb $ Cons fc $ Cons fd Nil

-- | Turn a tuple into a 'Product'.
--
-- >>> tupleToProduct (Identity 1, Identity 2.0) :: Product Identity '[Int, Double]
-- Cons (Identity 1) (Cons (Identity 2.0) Nil)
tupleToProduct :: ToProduct t f as => t -> Product f as
tupleToProduct = toProduct

-----------------
-- OpenProduct --
-----------------

-- | @'Product' 'Identity'@ is used as a standard open product type.
type OpenProduct = Product Identity

-- | 'ToOpenProduct' gives us a way to convert a tuple to an 'OpenProduct'.
-- See 'tupleToOpenProduct'.
class ToOpenProduct (tuple :: *) (as :: [*]) | as -> tuple where
  toOpenProduct :: tuple -> OpenProduct as

instance forall (a :: *). ToOpenProduct a '[a] where
  toOpenProduct :: a -> OpenProduct '[a]
  toOpenProduct a = Cons (Identity a) Nil

instance
    forall (a :: *) (b :: *). ToOpenProduct (a, b) '[a, b] where
  toOpenProduct :: (a, b) -> OpenProduct '[a, b]
  toOpenProduct (a, b) = Cons (Identity a) $ Cons (Identity b) Nil

instance
    forall (a :: *) (b :: *) (c :: *). ToOpenProduct (a, b, c) '[a, b, c] where
  toOpenProduct :: (a, b, c) -> OpenProduct '[a, b, c]
  toOpenProduct (a, b, c) =
    Cons (Identity a) $ Cons (Identity b) $ Cons (Identity c) Nil

instance
    forall (a :: *) (b :: *) (c :: *) (d :: *).
    ToOpenProduct (a, b, c, d) '[a, b, c, d] where
  toOpenProduct :: (a, b, c, d) -> OpenProduct '[a, b, c, d]
  toOpenProduct (a, b, c, d) =
    Cons (Identity a)
      . Cons (Identity b)
      . Cons (Identity c)
      $ Cons (Identity d) Nil

-- | Turn a tuple into an 'OpenProduct'.
--
-- For example, turn a triple into an 'OpenProduct':
--
-- >>> tupleToOpenProduct (1, 2.0, "hello") :: OpenProduct '[Int, Double, String]
-- Cons (Identity 1) (Cons (Identity 2.0) (Cons (Identity "hello") Nil))
--
-- Turn a single value into an 'OpenProduct':
--
-- >>> tupleToOpenProduct 'c' :: OpenProduct '[Char]
-- Cons (Identity 'c') Nil
tupleToOpenProduct :: ToOpenProduct t as => t -> OpenProduct as
tupleToOpenProduct = toOpenProduct

---------------
-- Instances --
---------------

instance Show (Product f '[]) where
  show :: Product f '[] -> String
  show Nil = "Nil"

instance (Show (f a), Show (Product f as)) => Show (Product f (a ': as)) where
  showsPrec :: Int -> (Product f (a ': as)) -> String -> String
  showsPrec n (Cons fa prod) = showParen (n > 10) $
    showString "Cons " . showsPrec 11 fa . showString " " . showsPrec 11 prod

