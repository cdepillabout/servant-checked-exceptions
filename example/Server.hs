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

module Main where

import Data.Char (toLower)
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Handler, (:<|>)((:<|>)), ServerT, serve)

import Servant.Checked.Exceptions
       (Envelope, pureErrEnvelope, pureSuccEnvelope)
import Servant.Checked.Exceptions.Internal.Servant.API (ErrStatus(toErrStatus), GetWithEx)

import Api
       (Api, BadSearchTermErr(BadSearchTermErr),
        IncorrectCapitalization(IncorrectCapitalization),
        SearchQuery(SearchQuery), SearchResponse, port)

-- | This is our server root for the 'ServerT' for 'Api'.  We only have two
-- handlers, 'postStrictSearch' and 'postLaxSearch'.
serverRoot :: ServerT Api Handler
serverRoot = postStrictSearch :<|> postLaxSearch :<|> postNoErrSearch :<|> testtesttest

-- | This is the handler for 'Api.ApiStrictSearch'.
--
-- If we get the 'SearchQuery' @\"Hello\"@, we return a 'SuccEnvelope'.
-- However, if we get a search query like @\"hello\"@, we return an
-- 'ErrEnvelope' with an 'IncorrectCapitialization' error.  If we get a search
-- query that is not @\"hello\"@, we return an 'ErrEnvelope' with a
-- 'BadSearchTermErr'.
--
-- Notice how we can use the polymorphic function 'pureErrEnvelope' to return
-- either an 'IncorrectCapitialization' error, or a 'BadSearchTermErr', even
-- though these two have different types.
--
-- Also, notice how this function returns an 'Envelope' because we are using
-- 'Throws' in the api definition.
postStrictSearch
  :: SearchQuery
  -> Handler (Envelope '[BadSearchTermErr, IncorrectCapitalization] SearchResponse)
postStrictSearch (SearchQuery "Hello") = pureSuccEnvelope "good"
postStrictSearch (SearchQuery query)
  | fmap toLower query == "hello" = pureErrEnvelope IncorrectCapitalization
  | otherwise = pureErrEnvelope BadSearchTermErr

-- | This is the handler for 'Api.ApiLaxSearch'.
--
-- This is similar to 'postStrictSearch', but it doesn't require correct
-- capitalization.
postLaxSearch
  :: SearchQuery
  -> Handler (Envelope '[BadSearchTermErr] SearchResponse)
postLaxSearch (SearchQuery query)
  | fmap toLower query == "hello" = pureSuccEnvelope "good"
  | otherwise = pureErrEnvelope BadSearchTermErr

-- | This is the handler for 'Api.ApiNoErrSearch'.
--
-- This is similar to 'postLaxSearch', but it doesn't require a correct search
-- term.
postNoErrSearch :: SearchQuery -> Handler (Envelope '[] SearchResponse)
postNoErrSearch (SearchQuery _) = pureSuccEnvelope "good"

testtesttest :: SearchQuery -> Handler (Envelope '[BadSearchTermErr, IncorrectCapitalization] SearchResponse)
testtesttest (SearchQuery _) = pureErrEnvelope IncorrectCapitalization

-- | Create a WAI 'Application'.
app :: Application
app = serve (Proxy :: Proxy Api) serverRoot

-- | Run the WAI 'Application' using 'run' on the port defined by 'port'.
main :: IO ()
main = run port app
