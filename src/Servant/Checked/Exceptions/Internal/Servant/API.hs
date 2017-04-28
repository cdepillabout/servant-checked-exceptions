{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Servant.Checked.Exceptions.Internal.Servant.API where

-- | 'Throws' is used in Servant API definitions and signifies that an API will
-- throw the given error.
--
-- Here is an example of how to create an API that potentially returns a
-- 'String' as an error, or an 'Int' on success:
--
-- >>> import Servant.API (Get, JSON, (:>))
-- >>> type API = Throws String :> Get '[JSON] Int
data Throws (e :: *)

-- | This is used internally and should not be used by end-users.
data Throwing (e :: [*])
