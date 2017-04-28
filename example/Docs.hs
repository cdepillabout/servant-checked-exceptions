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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Servant.API (Capture)
import Servant.Docs
       (DocCapture(DocCapture), ToCapture(toCapture), ToSample(toSamples),
        docs, markdown)

import Servant.Checked.Exceptions ()

import Api
       (Api, BadSearchTermErr(BadSearchTermErr),
        IncorrectCapitalization(IncorrectCapitalization), SearchQuery,
        SearchResponse)

instance ToSample SearchResponse where
  toSamples :: Proxy SearchResponse -> [(Text, SearchResponse)]
  toSamples Proxy = [("This is a successful response.", "good")]

instance ToCapture (Capture "query" SearchQuery) where
  toCapture :: Proxy (Capture "query" SearchQuery) -> DocCapture
  toCapture Proxy =
    DocCapture "query" "a search string like \"hello\" or \"bye\""

instance ToSample BadSearchTermErr where
  toSamples :: Proxy BadSearchTermErr -> [(Text, BadSearchTermErr)]
  toSamples Proxy =
    [("a completely incorrect search term was used", BadSearchTermErr)]

instance ToSample IncorrectCapitalization where
  toSamples :: Proxy IncorrectCapitalization -> [(Text, IncorrectCapitalization)]
  toSamples Proxy =
    [ ( "the search term \"Hello\" has not been capitalized correctly"
      , IncorrectCapitalization)
    ]

main :: IO ()
main = putStrLn . markdown $ docs (Proxy :: Proxy Api)
