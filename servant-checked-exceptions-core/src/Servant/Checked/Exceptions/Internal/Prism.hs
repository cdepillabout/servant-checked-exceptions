{-# LANGUAGE RankNTypes #-}

{- |
Module      :  Servant.Checked.Exceptions.Internal.Envelope
License     :  BSD3
Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

These functions are for working with Optics popularized by the
<https://hackage.haskell.org/package/lens lens> package. Documentation can be
found in the lens package.  These functions are redefined here to remove the
dependency on the lens package.
-}

module Servant.Checked.Exceptions.Internal.Prism
  ( Prism
  , prism
  , Prism'
  , prism'
  , Iso
  , iso
  , review
  , preview
  , (<>~)
  ) where

import Data.Profunctor.Unsafe((#.))
import Control.Applicative
import Data.Coerce
import Data.Functor.Identity
import Data.Monoid
import Data.Profunctor
import Data.Tagged

type Iso s t a b
   = forall p f. (Profunctor p, Functor f) =>
                   p a (f b) -> p s (f t)

type Prism s t a b
   = forall p f. (Choice p, Applicative f) =>
                   p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

type ASetter s t a b = (a -> Identity b) -> s -> Identity t

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

prism' :: (a -> s) -> (s -> Maybe a) -> Prism' s a
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
{-# INLINE prism' #-}

review :: Prism' t b -> b -> t
review p = coerce . p . Tagged . Identity
{-# INLINE review #-}

preview :: Prism' s a -> s -> Maybe a
preview l = coerce . l (Const . First . Just)
{-# INLINE preview #-}

over :: ASetter s t a b -> (a -> b) -> s -> t
over l f = runIdentity #. l (Identity #. f)
{-# INLINE over #-}

infixr 4 <>~
(<>~) :: Monoid a => ASetter s t a a -> a -> s -> t
l <>~ n = over l (`mappend` n)
{-# INLINE (<>~) #-}
