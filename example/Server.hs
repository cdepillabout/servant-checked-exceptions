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

import Api
       (Api, BadSearchTermErr(BadSearchTermErr),
        IncorrectCapitalization(IncorrectCapitalization),
        SearchQuery(SearchQuery), SearchResponse, port)

serverRoot :: ServerT Api Handler
serverRoot = postStrictSearch :<|> postLaxSearch

postStrictSearch
  :: SearchQuery
  -> Handler (Envelope '[BadSearchTermErr, IncorrectCapitalization] SearchResponse)
postStrictSearch (SearchQuery "Hello") = pureSuccEnvelope "good"
postStrictSearch (SearchQuery query)
  | fmap toLower query == "hello" = pureErrEnvelope IncorrectCapitalization
  | otherwise = pureErrEnvelope BadSearchTermErr

postLaxSearch
  :: SearchQuery
  -> Handler (Envelope '[BadSearchTermErr] SearchResponse)
postLaxSearch (SearchQuery query)
  | fmap toLower query == "hello" = pureSuccEnvelope "good"
  | otherwise = pureErrEnvelope BadSearchTermErr

app :: Application
app = serve (Proxy :: Proxy Api) serverRoot

main :: IO ()
main = run port app
