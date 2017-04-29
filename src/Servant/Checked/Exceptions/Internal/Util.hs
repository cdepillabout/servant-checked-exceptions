{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
-- >>> import Data.Type.Equality ((:~:)(Refl))
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

type family ReturnX x as where
  ReturnX x (a ': as) = ((a -> x) ': ReturnX x as)
  ReturnX x '[] = '[]

