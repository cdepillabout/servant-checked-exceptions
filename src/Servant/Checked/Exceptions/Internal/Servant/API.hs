{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Servant.Checked.Exceptions.Internal.Servant.API where

data Throws (e :: *)

data Throwing (e :: [*])
