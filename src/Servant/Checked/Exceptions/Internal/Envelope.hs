{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Checked.Exceptions.Internal.Envelope where

import Control.Applicative ((<|>))
import Data.Aeson
       (FromJSON(parseJSON), ToJSON(toJSON), Value, (.=), (.:), object, withObject)
import Data.Aeson.Types (Parser)

import Servant.Checked.Exceptions.Internal.Union (IsMember, OpenUnion, openUnionLift)

data Envelope es a = ErrEnvelope (OpenUnion es) | SuccEnvelope a

toErrEnvelope :: IsMember e es => e -> Envelope es a
toErrEnvelope = ErrEnvelope . openUnionLift

toSuccEnvelope :: a -> Envelope es a
toSuccEnvelope = SuccEnvelope

pureErrEnvelope :: (Applicative m, IsMember e es) => e -> m (Envelope es a)
pureErrEnvelope = pure . toErrEnvelope

pureSuccEnvelope :: Applicative m => a -> m (Envelope es a)
pureSuccEnvelope = pure . toSuccEnvelope

instance (ToJSON (OpenUnion es), ToJSON a) => ToJSON (Envelope es a) where
  toJSON :: Envelope es a -> Value
  toJSON (ErrEnvelope es) = object ["err" .= es]
  toJSON (SuccEnvelope a) = object ["data" .= a]

instance (FromJSON (OpenUnion es), FromJSON a) => FromJSON (Envelope es a) where
  parseJSON :: Value -> Parser (Envelope es a)
  parseJSON = withObject "Envelope" $ \obj ->
    SuccEnvelope <$> obj .: "data" <|>
    ErrEnvelope <$> obj .: "err"
