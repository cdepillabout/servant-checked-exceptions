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

module Servant.Checked.Exceptions.Internal.Product
  where

import Data.Functor.Identity (Identity(Identity))

-- $setup
-- >>> -- :set -XDataKinds

data Product (f :: u -> *) (as :: [u]) where
  Nil :: Product f '[]
  Cons :: !(f a) -> Product f as -> Product f (a ': as)

class ToProductF (tuple :: *) (f :: u -> *) (as :: [u]) | f as -> tuple where
  toProductF :: tuple -> Product f as

instance forall (f :: u -> *) (a :: u). ToProductF (f a) f '[a] where
  toProductF :: f a -> Product f '[a]
  toProductF fa = Cons fa Nil

instance forall (f :: u -> *) (a :: u) (b :: u). ToProductF (f a, f b) f '[a, b] where
  toProductF :: (f a, f b) -> Product f '[a, b]
  toProductF (fa, fb) = Cons fa $ Cons fb Nil

instance forall (f :: u -> *) (a :: u) (b :: u) (c :: u). ToProductF (f a, f b, f c) f '[a, b, c] where
  toProductF :: (f a, f b, f c) -> Product f '[a, b, c]
  toProductF (fa, fb, fc) = Cons fa $ Cons fb $ Cons fc Nil

instance forall (f :: u -> *) (a :: u) (b :: u) (c :: u) (d :: u). ToProductF (f a, f b, f c, f d) f '[a, b, c, d] where
  toProductF :: (f a, f b, f c, f d) -> Product f '[a, b, c, d]
  toProductF (fa, fb, fc, fd) = Cons fa $ Cons fb $ Cons fc $ Cons fd Nil

tupleToProductF :: ToProductF t f as => t -> Product f as
tupleToProductF = toProductF

class ToProduct (tuple :: *) (as :: [*]) | as -> tuple where
  toProduct :: tuple -> Product Identity as

instance forall (a :: *). ToProduct a '[a] where
  toProduct :: a -> Product Identity '[a]
  toProduct a = Cons (Identity a) Nil

instance forall (a :: *) (b :: *). ToProduct (a, b) '[a, b] where
  toProduct :: (a, b) -> Product Identity '[a, b]
  toProduct (a, b) = Cons (Identity a) $ Cons (Identity b) Nil

instance forall (a :: *) (b :: *) (c :: *). ToProduct (a, b, c) '[a, b, c] where
  toProduct :: (a, b, c) -> Product Identity '[a, b, c]
  toProduct (a, b, c) = Cons (Identity a) $ Cons (Identity b) $ Cons (Identity c) Nil

instance forall (a :: *) (b :: *) (c :: *) (d :: *). ToProduct (a, b, c, d) '[a, b, c, d] where
  toProduct :: (a, b, c, d) -> Product Identity '[a, b, c, d]
  toProduct (a, b, c, d) = Cons (Identity a) $ Cons (Identity b) $ Cons (Identity c) $ Cons (Identity d) Nil

tupleToProduct :: ToProduct t as => t -> Product Identity as
tupleToProduct = toProduct

