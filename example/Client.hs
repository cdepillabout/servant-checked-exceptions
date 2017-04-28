{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Options.Applicative
       (Parser, (<**>), execParser, fullDesc, helper, info, progDesc)
import Servant.API ((:<|>)((:<|>)))
import Servant.Client
       (BaseUrl(BaseUrl), ClientEnv(ClientEnv), ClientM, Scheme(Http),
        client, runClientM)

import Servant.Checked.Exceptions (Envelope)

import Api
       (Api, BadSearchTermErr, IncorrectCapitalization,
        SearchQuery(SearchQuery), SearchResponse, port)

strictSearch
  :: SearchQuery
  -> ClientM (Envelope '[BadSearchTermErr, IncorrectCapitalization] SearchResponse)
laxSearch
  :: SearchQuery
  -> ClientM (Envelope '[BadSearchTermErr] SearchResponse)
strictSearch :<|> laxSearch = client (Proxy :: Proxy Api)

data Options = Options { query :: String, useStrict :: Bool }

queryParser :: Parser String
queryParser = undefined

useStrictParser :: Parser Bool
useStrictParser = undefined

commandParser :: Parser Options
commandParser = Options <$> queryParser <*> useStrictParser

run :: ClientEnv -> Options -> IO ()
run clientEnv Options{query, useStrict = True} = do
  eitherRes <- runClientM (strictSearch $ SearchQuery query) clientEnv
  case eitherRes of
    Left servantErr -> putStrLn $ "Got a ServantErr: " <> show servantErr
run clientEnv Options{query, useStrict = False} = do
  eitherRes <- runClientM (laxSearch $ SearchQuery query) clientEnv
  case eitherRes of
    Left servantErr -> putStrLn $ "Got a ServantErr: " <> show servantErr

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" port ""

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let clientEnv = ClientEnv manager baseUrl
  options <- execParser opts
  run clientEnv options
  where
    opts = info (commandParser <**> helper) $
      fullDesc <> progDesc "Print a greeting for TARGET"
