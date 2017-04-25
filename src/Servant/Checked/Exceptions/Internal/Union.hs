{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Checked.Exceptions.Internal.Union where

-- Imports for Union stuff
import Control.Applicative ((<|>))
import Control.Lens (Prism, Prism', iso, preview, prism, prism', review)
import Control.DeepSeq (NFData(rnf))
import Data.Aeson
       (FromJSON(parseJSON), ToJSON(toJSON), Value)
import Data.Aeson.Types (Parser)
import Data.Functor.Identity (Identity(Identity, runIdentity))
import Data.Typeable (Typeable)
import GHC.TypeLits (Nat, type (+))
import Text.Read (Read(readPrec), ReadPrec)

---------------------------------------
-- This is from Data.Vinyl.TypeLevel --
---------------------------------------

-- | A partial relation that gives the index of a value in a list.
type family RIndex (r :: k) (rs :: [k]) :: Nat where
  RIndex r (r ': rs) = 0
  RIndex r (s ': rs) = 1 + (RIndex r rs)

-----------------------------
-- This is from Data.Union --
-----------------------------

-- | A union is parameterized by a universe @u@, an interpretation @f@
-- and a list of labels @as@. The labels of the union are given by
-- inhabitants of the kind @u@; the type of values at any label @a ::
-- u@ is given by its interpretation @f a :: *@.
data Union (f :: u -> *) (as :: [u]) where
  This :: !(f a) -> Union f (a ': as)
  That :: !(Union f as) -> Union f (a ': as)
  deriving (Typeable)

-- | Case analysis for unions.
union :: (Union f as -> c) -> (f a -> c) -> Union f (a ': as) -> c
union _ onThis (This a) = onThis a
union onThat _ (That u) = onThat u

-- | Since a union with an empty list of labels is uninhabited, we
-- can recover any type from it.
absurdUnion :: Union f '[] -> a
absurdUnion u = case u of {}

umap :: (forall a . f a -> g a) -> Union f as -> Union g as
umap f (This a) = This $ f a
umap f (That u) = That $ umap f u

_This :: Prism (Union f (a ': as)) (Union f (b ': as)) (f a) (f b)
_This = prism This (union (Left . That) Right)
{-# INLINE _This #-}

_That :: Prism (Union f (a ': as)) (Union f (a ': bs)) (Union f as) (Union f bs)
_That = prism That (union Right (Left . This))
{-# INLINE _That #-}

class i ~ RIndex a as => UElem (a :: u) (as :: [u]) (i :: Nat) where
  {-# MINIMAL unionPrism | unionLift, unionMatch #-}

  unionPrism :: Prism' (Union f as) (f a)
  unionPrism = prism' unionLift unionMatch

  unionLift :: f a -> Union f as
  unionLift = review unionPrism

  unionMatch :: Union f as -> Maybe (f a)
  unionMatch = preview unionPrism

instance UElem a (a ': as) 0 where
  unionPrism :: Prism' (Union f (a ': as)) (f a)
  unionPrism = _This
  {-# INLINE unionPrism #-}

instance {-# OVERLAPPABLE #-} (RIndex a (b ': as) ~ n, UElem a as i, n ~ (1 + i))
    => UElem a (b ': as) n where
  unionPrism :: Prism' (Union f (b ': as)) (f a)
  unionPrism = _That . unionPrism
  {-# INLINE unionPrism #-}

type IsMember a as = UElem a as (RIndex a as)

type OpenUnion = Union Identity

-- | Case analysis for 'OpenUnion'.
openUnion
  :: forall as a c.
     (OpenUnion as -> c) -> (a -> c) -> OpenUnion (a ': as) -> c
openUnion onThat onThis = union onThat (onThis . runIdentity)

openUnionPrism
  :: forall a as.
     IsMember a as
  => Prism' (OpenUnion as) a
openUnionPrism = unionPrism . iso runIdentity Identity
{-# INLINE openUnionPrism #-}

openUnionMatch
  :: forall a as.
     IsMember a as
  => OpenUnion as -> Maybe a
openUnionMatch = preview openUnionPrism

openUnionLift
  :: forall a as.
     IsMember a as
  => a -> OpenUnion as
openUnionLift = review openUnionPrism

instance NFData (Union f '[]) where
  rnf = absurdUnion

instance (NFData (f a), NFData (Union f as)) => NFData (Union f (a ': as)) where
  rnf = union rnf rnf

instance Show (Union f '[]) where
  showsPrec _ = absurdUnion

instance (Show (f a), Show (Union f as)) => Show (Union f (a ': as)) where
  showsPrec n = union (showsPrec n) (showsPrec n)

-- | This will always fail, since @'Union' f \'[]@ is effectively 'Void'.
instance Read (Union f '[]) where
  readsPrec :: Int -> ReadS (Union f '[])
  readsPrec _ _ = []

-- | TODO: This is only a valid instance when the 'Read' instances for the types don't overlap.
instance (Read (f a), Read (Union f as)) => Read (Union f (a ': as)) where
  readPrec :: ReadPrec (Union f (a ': as))
  readPrec = fmap This readPrec <|> fmap That readPrec

instance Eq (Union f '[]) where
  (==) = absurdUnion

instance (Eq (f a), Eq (Union f as)) => Eq (Union f (a ': as)) where
    This a1 == This a2 = a1 == a2
    That u1 == That u2 = u1 == u2
    _       == _       = False

instance Ord (Union f '[]) where
  compare = absurdUnion

instance (Ord (f a), Ord (Union f as)) => Ord (Union f (a ': as))
  where
    compare (This a1) (This a2) = compare a1 a2
    compare (That u1) (That u2) = compare u1 u2
    compare (This _)  (That _)  = LT
    compare (That _)  (This _)  = GT

instance ToJSON (Union f '[]) where
  toJSON :: Union f '[] -> Value
  toJSON = absurdUnion

instance (ToJSON (f a), ToJSON (Union f as)) => ToJSON (Union f (a ': as)) where
  toJSON :: Union f (a ': as) -> Value
  toJSON = union toJSON toJSON

-- | This will always fail, since @'Union' f \'[]@ is effectively 'Void'.
instance FromJSON (Union f '[]) where
  parseJSON :: Value -> Parser (Union f '[])
  parseJSON _ = fail "Value of Union f '[] can never be created"

-- | TODO: This is only a valid instance when the 'Read' instances for the types don't overlap.
instance (FromJSON (f a), FromJSON (Union f as)) => FromJSON (Union f (a ': as)) where
  parseJSON :: Value -> Parser (Union f (a ': as))
  parseJSON val = fmap This (parseJSON val) <|> fmap That (parseJSON val)

-- instance f ~ Identity => Exception (Union f '[])

-- instance
--     ( f ~ Identity
--     , Exception a
--     , Typeable as
--     , Exception (Union f as)
--     ) => Exception (Union f (a ': as))
--   where
--     toException = union toException (toException . runIdentity)
--     fromException sE = matchR <|> matchL
--       where
--         matchR = This . Identity <$> fromException sE
--         matchL = That <$> fromException sE
