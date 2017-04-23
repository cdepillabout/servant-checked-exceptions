{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Checked.Exceptions where

-- Imports for Union stuff
import Control.Lens (Prism, Prism', iso, preview, prism, prism', review)
import Control.DeepSeq (NFData(rnf))
import Data.Functor.Identity (Identity(Identity, runIdentity))
import GHC.TypeLits (Nat, type (+))

-- Imports for Servant Stuff
import Data.Aeson (ToJSON(toJSON), Value, (.=), object)
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant.Server.Internal.Router (Router)
import Servant.Server.Internal.RoutingApplication (Delayed)
import Servant
       (Context, Get, Handler, HasServer(..), JSON, Post, QueryParam,
        Server, ServerT, Verb, (:>), (:<|>)((:<|>)), enter,
        serve)

-- This changes in servant-0.10
-- import Control.Natural ((:~>)(NT))
import Servant.Utils.Enter ((:~>)(Nat))

-- ghci> let a = openUnion # (5 :: Int) :: OpenUnion '[Bool, Int]

-- ghci> a ^? openUnion :: Maybe Int
-- Just 5

-- ghci> a ^? openUnion :: Maybe Bool
-- Nothing

-- ghci> a ^? openUnion :: Maybe Char
-- <interactive>:8:6:
--     No instance for (UElem Char '[] (RIndex Char '[]))
--       arising from a use of ‘openUnion’
--     In the second argument of ‘(^?)’, namely ‘openUnion’
--     In the expression: a ^? openUnion :: Maybe Char
--     In an equation for ‘it’: it = a ^? openUnion :: Maybe Char

-----------
-- TESTS --
-----------

testThisUnion :: Union Maybe (Int ': xs)
testThisUnion = This $ Just 3

testThatUnion :: Union Maybe (x ': Int ': as)
testThatUnion = That . This $ Just 3

unionTest :: String
unionTest =
  union
    (const "that")
    (\maybeInt -> "this: " ++ show maybeInt)
    testThisUnion

-- recursiveUnion :: String
-- recursiveUnion =
--   union (\u -> undefined) (\m -> "this: " ++ show m) testThatUnion

testUlift :: Union Identity (String ': Char ': Int ': as)
testUlift = ulift $ Identity (3 :: Int)

testUmatch :: Maybe (Identity String)
testUmatch = umatch testUlift

testOpenUnion :: OpenUnion '[String, Int]
testOpenUnion = review openUnion (3 :: Int)

testOpenUnion2 :: OpenUnion '[String, Int]
testOpenUnion2 = review openUnion ("hello" :: String)

testGetFromOpenUnion :: Maybe Int
testGetFromOpenUnion = matchOpenUnion testOpenUnion

testGetFromOpenUnion2 :: Maybe String
testGetFromOpenUnion2 = matchOpenUnion testOpenUnion

---------------------------------------
-- This is from Data.Vinyl.TypeLevel --
---------------------------------------

-- | A partial relation that gives the index of a value in a list.
type family RIndex (r :: k) (rs :: [k]) :: Nat where
  RIndex r (r ': rs) = 0
  RIndex r (s ': rs) = 1 + (RIndex r rs)

-- | A partial relation that gives the indices of a sublist in a larger list.
type family RImage (rs :: [k]) (ss :: [k]) :: [Nat] where
  RImage '[] ss = '[]
  RImage (r ': rs) ss = RIndex r ss ': RImage rs ss

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

-- | Case analysis for unions.
union :: (Union f as -> c) -> (f a -> c) -> Union f (a ': as) -> c
union onThat onThis = \case
  This a -> onThis a
  That u -> onThat u

-- | Since a union with an empty list of labels is uninhabited, we
-- can recover any type from it.
absurdUnion :: Union f '[] -> a
absurdUnion = \case{}

umap :: (forall a . f a -> g a) -> Union f as -> Union g as
umap f = \case
  This a -> This (f a)
  That u -> That (umap f u)

_This :: Prism (Union f (a ': as)) (Union f (b ': as)) (f a) (f b)
_This = prism This (union (Left . That) Right)
{-# INLINE _This #-}

_That :: Prism (Union f (a ': as)) (Union f (a ': bs)) (Union f as) (Union f bs)
_That = prism That (union Right (Left . This))
{-# INLINE _That #-}

class i ~ RIndex a as => UElem (a :: u) (as :: [u]) (i :: Nat) where
  {-# MINIMAL uprism | ulift, umatch #-}

  uprism :: Prism' (Union f as) (f a)
  uprism = prism' ulift umatch

  ulift :: f a -> Union f as
  ulift = review uprism

  umatch :: Union f as -> Maybe (f a)
  umatch = preview uprism

instance UElem a (a ': as) 0 where
  uprism :: Prism' (Union f (a ': as)) (f a)
  uprism = _This
  {-# INLINE uprism #-}

instance {-# OVERLAPPABLE #-} (RIndex a (b ': as) ~ n, UElem a as i, n ~ (1 + i))
    => UElem a (b ': as) n where
  uprism :: Prism' (Union f (b ': as)) (f a)
  uprism = _That . uprism
  {-# INLINE uprism #-}

type OpenUnion = Union Identity

openUnion :: forall a as . UElem a as (RIndex a as) => Prism' (OpenUnion as) a
openUnion = uprism . iso runIdentity Identity
{-# INLINE openUnion #-}

matchOpenUnion :: forall a as . UElem a as (RIndex a as) => OpenUnion as -> Maybe a
matchOpenUnion = preview openUnion

openUnionLift :: UElem a as (RIndex a as) => a -> OpenUnion as
openUnionLift = review openUnion

type IsMember a as = UElem a as (RIndex a as)

instance NFData (Union f '[]) where
  rnf = absurdUnion

instance (NFData (f a), NFData (Union f as)) => NFData (Union f (a ': as)) where
  rnf = union rnf rnf

instance Show (Union f '[]) where
  showsPrec _ = absurdUnion

instance (Show (f a), Show (Union f as)) => Show (Union f (a ': as)) where
  showsPrec n = union (showsPrec n) (showsPrec n)

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

-------------
-- Servant --
-------------

defaultMainApi :: IO ()
defaultMainApi = run 8201 app

type Api = ApiSearch :<|> ApiStatus

type ApiSearch =
  "search" :>
  QueryParam "q" String :>
  Throws FooErr :>
  Throws BarErr :>
  -- Throwing '[FooErr, BarErr] :>
  -- Post '[JSON] (Envelope '[FooErr, BarErr] String)
  Post '[JSON] String

type ApiStatus = "status" :> Get '[JSON] Int

serverRoot :: ServerT Api Handler
serverRoot = search :<|> status

search :: Maybe String -> Handler (Envelope '[FooErr, BarErr] String)
-- search :: Maybe String -> Handler (Envelope '[FooErr] String)
search maybeQ = do
  case maybeQ of
    Just "hello" -> pureErrEnvelope BarErr
    Just "Hello" -> pureSuccEnvelope "good"
    _ -> pureErrEnvelope FooErr

status :: Handler Int
status = pure 1

-- | Given a 'Config', this returns a Wai 'Application'.
app :: Application
app = serve (Proxy :: Proxy Api) apiServer

-- | Given a 'Config', this returns a servant 'Server' for 'Api'
apiServer :: Server Api
apiServer = enter natTrans serverRoot
  where
    natTrans :: Handler :~> Handler
    natTrans = Nat trans

    trans :: forall a. Handler a -> Handler a
    trans = id

------------------------
-- Servant Type-Level --
------------------------

data Throws (e :: *)

data Throwing (e :: [*])

-- TODO: Make sure to also account for when headers are being used.

instance (HasServer (Throwing '[e] :> api) context) =>
    HasServer (Throws e :> api) context where

  type ServerT (Throws e :> api) m =
    ServerT (Throwing '[e] :> api) m

  route
    :: Proxy (Throws e :> api)
    -> Context context
    -> Delayed env (ServerT (Throwing '[e] :> api) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy (Throwing '[e] :> api))

instance (HasServer (Verb method status ctypes (Envelope es a)) context) =>
    HasServer (Throwing es :> Verb method status ctypes a) context where

  type ServerT (Throwing es :> Verb method status ctypes a) m =
    ServerT (Verb method status ctypes (Envelope es a)) m

  route
    :: Proxy (Throwing es :> Verb method status ctypes a)
    -> Context context
    -> Delayed env (ServerT (Verb method status ctypes (Envelope es a)) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy (Verb method status ctypes (Envelope es a)))

instance (HasServer (Throwing (Snoc es e) :> api) context) =>
    HasServer (Throwing es :> Throws e :> api) context where

  type ServerT (Throwing es :> Throws e :> api) m =
    ServerT (Throwing (Snoc es e) :> api ) m

  route
    :: Proxy (Throwing es :> Throws e :> api)
    -> Context context
    -> Delayed env (ServerT (Throwing (Snoc es e) :> api) Handler)
    -> Router env
  route _ = route (Proxy :: Proxy (Throwing (Snoc es e) :> api))

type family Snoc (as :: [k]) (b :: k) where
  Snoc '[] b = '[b]
  Snoc (a ': as) b = (a ': Snoc as b)

------------
-- Errors --
------------

data FooErr = FooErr deriving (Eq, Read, Show)

instance ToJSON FooErr where
  toJSON :: FooErr -> Value
  toJSON = toJSON . show

data BarErr = BarErr deriving (Eq, Read, Show)

instance ToJSON BarErr where
  toJSON :: BarErr -> Value
  toJSON = toJSON . show

data BazErr = BazErr deriving (Eq, Read, Show)

instance ToJSON BazErr where
  toJSON :: BazErr -> Value
  toJSON = toJSON . show

--------------
-- Envelope --
--------------

-- data Envelope' e a = ErrEnvelope e | SuccEnvelope a
-- type Envelope e = Envelope' (OpenUnion e)

data Envelope e a = ErrEnvelope (OpenUnion e) | SuccEnvelope a

toErrEnvelope :: IsMember e es => e -> Envelope es a
toErrEnvelope = ErrEnvelope . openUnionLift

toSuccEnvelope :: a -> Envelope e a
toSuccEnvelope = SuccEnvelope

pureErrEnvelope :: (Applicative m, IsMember e es) => e -> m (Envelope es a)
pureErrEnvelope = pure . toErrEnvelope

pureSuccEnvelope :: Applicative m => a -> m (Envelope e a)
pureSuccEnvelope = pure . toSuccEnvelope

instance (ToJSON (OpenUnion e), ToJSON a) => ToJSON (Envelope e a) where
  toJSON :: Envelope e a -> Value
  toJSON (ErrEnvelope e) = object ["err" .= e]
  toJSON (SuccEnvelope a) = object ["data" .= a]
