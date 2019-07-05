{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      :  Servant.Checked.Exceptions.Internal.Util

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

Additional helpers.
-}

module Servant.Checked.Exceptions.Internal.Util where

-- | A type-level @snoc@.
--
-- Append to an empty list:
--
-- >>> Refl :: Snoc '[] Double :~: '[Double]
-- Refl
--
-- Append to a non-empty list:
--
-- >>> Refl :: Snoc '[Char] String :~: '[Char, String]
-- Refl
type family Snoc (as :: [k]) (b :: k) where
  Snoc '[] b = '[b]
  Snoc (a ': as) b = (a ': Snoc as b)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> import Data.Type.Equality ((:~:)(Refl))

-- type family Unique (xs :: [k]) :: [k] where
--   Unique '[] = '[]
--   Unique (a ': as) = UniqueHelper a as

-- type family UniqueHelper (xs :: [k]) (ys :: [k]) :: [k] where
--   UniqueHelper '[] '[] = '[]
--   UniqueHelper '[] (a ': as) = UniqueHelper '[a] as
--   UniqueHelper (a ': as) '[] = (a ': as)
--   UniqueHelper as (b ': bs) = If (Elem b as) (UniqueHelper as bs) (UniqueHelper (Snoc as b) bs)

-- type family Elem

type family UniqueAppend (xs :: [k]) (ys :: [k]) :: [k] where
  UniqueAppend '[] ys = ys
  UniqueAppend xs '[] = xs
  UniqueAppend xs (y ': ys) = If (Elem y xs) (UniqueAppend xs ys) (UniqueAppend (Snoc xs y) ys)

type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem _ '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (y ': xs) = Elem x xs

type family If (b :: Bool) (thenCase :: k) (elseCase :: k) :: k where
  If 'True thenCase _ = thenCase
  If 'False _ elseCase = elseCase
