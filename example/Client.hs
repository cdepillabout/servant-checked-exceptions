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

import Data.Proxy (Proxy(Proxy))
import Servant.API ((:<|>)((:<|>)))
import Servant.Client (ClientM, client)

import Servant.Checked.Exceptions (Envelope)

import Api
       (Api, BadSearchTermErr, IncorrectCapitalization, SearchQuery,
        SearchResponse)

------------
-- Client --
------------

strictSearch
  :: SearchQuery
  -> ClientM (Envelope '[BadSearchTermErr, IncorrectCapitalization] SearchResponse)
laxSearch
  :: SearchQuery
  -> ClientM (Envelope '[BadSearchTermErr] SearchResponse)
strictSearch :<|> laxSearch = client (Proxy :: Proxy Api)

main :: IO ()
main = undefined
