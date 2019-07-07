{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This example is very similar to @./Server.hs@, but it is making use of
-- 'EnvelopeT' instead of just 'Envelope'.

module Main where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Char (toLower)
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Handler, (:<|>)((:<|>)), ServerT, serve)

import Servant.Checked.Exceptions
  ( Envelope
  , EnvelopeT
  , IsMember
  , pureErrEnvelope
  , pureSuccEnvelope
  , relaxEnvT
  , runEnvelopeT
  , throwErrEnvT
  )

import Api
  ( Api
  , BadSearchTermErr(BadSearchTermErr)
  , IncorrectCapitalization(IncorrectCapitalization)
  , SearchQuery(SearchQuery)
  , SearchResponse
  , port
  )

-- | This is our server root for the 'ServerT' for 'Api'.  We only have two
-- handlers, 'postStrictSearch' and 'postLaxSearch'.
serverRoot :: ServerT Api Handler
serverRoot = postStrictSearch :<|> postLaxSearch :<|> postNoErrSearch

-- | This is the handler for 'Api.ApiStrictSearch'.
postStrictSearch
  :: SearchQuery
  -> Handler (Envelope '[BadSearchTermErr, IncorrectCapitalization] SearchResponse)
postStrictSearch query =
  runEnvelopeT $ do
    res <- checkQueryInDb query
    relaxEnvT $ doubleCheckCapitalization query
    doubleCheckCapitalizationGeneral query
    pure res

-- | Check that the input 'SearchQuery' is in the DB.  (The DB is just a list
-- of greetings.)
--
-- If the input 'SearchQuery' is in the DB, return the string @"greeting"@.
-- Otherwise, throw an envelope error 'BadSearchTermErr'.
--
-- Note that this function says that it might also return an
-- 'IncorrectCapitalization' error, but it doesn't actually.  This is just for
-- demonstration.
--
-- Also note that our underlying Monad here is 'Handler'.
checkQueryInDb
  :: SearchQuery
  -> EnvelopeT '[BadSearchTermErr, IncorrectCapitalization] Handler SearchResponse
checkQueryInDb (SearchQuery query) = do
  liftIO $ putStrLn "querying DB..."
  let lowerQuery = fmap toLower query
  case lowerQuery `elem` ["hello", "goodbye", "goodnight"] of
    True -> pure "greeting"
    False -> throwErrEnvT BadSearchTermErr

-- | Check the first letter of 'SearchQuery' to make sure that it is lowercase.
--
-- If it is not lowercase, throw 'IncorrectCapitalization'.
--
-- Note that 'relaxEnvT' needs to be called on the result of this to use it in
-- do notation with the above 'checkQueryInDb'.  This is because the result of
-- this is technically a different monad than the result of 'checkQueryInDb',
-- because the error types are different.
--
-- Also note that since we only use 'MonadIO' for this, our underlying monad is
-- polymorphic.  We don't specialize it to 'Handler'.
doubleCheckCapitalization
  :: MonadIO m => SearchQuery -> EnvelopeT '[IncorrectCapitalization] m ()
doubleCheckCapitalization (SearchQuery []) = liftIO $ putStrLn "search query empty"
doubleCheckCapitalization (SearchQuery (q:query)) =
  if toLower q == q
    then pure ()
    else throwErrEnvT IncorrectCapitalization

-- | This is just like 'doubleCheckCapitalization' above, but we use the
-- 'IsMember' constraint to make this function more general.  In
-- 'postStrictSearch', you can see that we don't have to call 'relaxEnvT' when
-- using this function.
doubleCheckCapitalizationGeneral
  :: IsMember IncorrectCapitalization es => SearchQuery -> EnvelopeT es Handler ()
doubleCheckCapitalizationGeneral _ = throwErrEnvT IncorrectCapitalization

-- | This doesn't do anything interesting.
--
-- See 'postLaxSearch' in @./Server.hs@ for an example using 'Envelope'.
postLaxSearch
  :: SearchQuery
  -> Handler (Envelope '[BadSearchTermErr] SearchResponse)
postLaxSearch _ = pureSuccEnvelope "good"

-- | This doesn't do anything interesting.
--
-- See 'postNoErrSearch' in @./Server.hs@ for an example using 'Envelope'.
postNoErrSearch :: SearchQuery -> Handler (Envelope '[] SearchResponse)
postNoErrSearch (SearchQuery _) = pureSuccEnvelope "good"

-- | Create a WAI 'Application'.
app :: Application
app = serve (Proxy :: Proxy Api) serverRoot

-- | Run the WAI 'Application' using 'run' on the port defined by 'port'.
main :: IO ()
main = run port app
