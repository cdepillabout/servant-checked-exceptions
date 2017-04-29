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
       (Parser, (<**>), argument, execParser, fullDesc, help, helper, info, long,
        metavar, progDesc, short, str, switch)
import Servant.API ((:<|>)((:<|>)))
import Servant.Client
       (BaseUrl(BaseUrl), ClientEnv(ClientEnv), ClientM, Scheme(Http),
        client, runClientM)

import Servant.Checked.Exceptions (Envelope, catchesEnvelope)

import Api
       (Api, BadSearchTermErr(BadSearchTermErr),
        IncorrectCapitalization(IncorrectCapitalization),
        SearchQuery(SearchQuery), SearchResponse(SearchResponse), port)

strictSearch
  :: SearchQuery
  -> ClientM (Envelope '[BadSearchTermErr, IncorrectCapitalization] SearchResponse)
laxSearch
  :: SearchQuery
  -> ClientM (Envelope '[BadSearchTermErr] SearchResponse)
strictSearch :<|> laxSearch = client (Proxy :: Proxy Api)

data Options = Options { query :: String, useStrict :: Bool }

queryParser :: Parser String
queryParser = argument str (metavar "QUERY")

useStrictParser :: Parser Bool
useStrictParser =
  switch $
    long "strict" <> short 's' <> help "Whether to be use the strict api"

commandParser :: Parser Options
commandParser = Options <$> queryParser <*> useStrictParser

runStrict :: ClientEnv -> String -> IO ()
runStrict clientEnv query = do
  eitherRes <- runClientM (strictSearch $ SearchQuery query) clientEnv
  case eitherRes of
    Left servantErr -> putStrLn $ "Got a ServantErr: " <> show servantErr
    Right env ->
      putStrLn $
        catchesEnvelope
          ( \BadSearchTermErr -> "the search term was not \"Hello\""
          , \IncorrectCapitalization -> "the search term was not capitalized correctly"
          )
          (\(SearchResponse searchResponse) -> "Success: " <> searchResponse)
          env

runLax :: ClientEnv -> String -> IO ()
runLax clientEnv query = do
  eitherRes <- runClientM (laxSearch $ SearchQuery query) clientEnv
  case eitherRes of
    Left servantErr -> putStrLn $ "Got a ServantErr: " <> show servantErr
    Right env ->
      putStrLn $
        catchesEnvelope
          (\BadSearchTermErr -> "the search term was not \"Hello\"")
          (\(SearchResponse searchResponse) -> "Success: " <> searchResponse)
          env

run :: ClientEnv -> Options -> IO ()
run clientEnv Options{query, useStrict = True} = runStrict clientEnv query
run clientEnv Options{query, useStrict = False} = runLax clientEnv query

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let clientEnv = ClientEnv manager baseUrl
  options <- execParser opts
  run clientEnv options
  where
    opts = info (commandParser <**> helper) $
      fullDesc <> progDesc "Print a greeting for TARGET"

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" port ""

