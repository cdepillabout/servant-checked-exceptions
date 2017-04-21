{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Spec.ServerSpec where

import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Servant.Server (serve)
import Test.Hspec.Wai
       (ResponseMatcher(matchHeaders), (<:>), get, shouldRespondWith,
        with)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, it)

import Servant.Static.TH (createApiAndServerDecs)

import Spec.TestDirLocation (testDir)

$(createApiAndServerDecs "FrontEndApi" "testDirServer" testDir)

app :: Application
app = serve (Proxy @FrontEndApi) testDirServer

serverTestsIO :: IO TestTree
serverTestsIO =
  testSpec "server" $
    with (pure app) $ do
      it "hello.html responds correctly and is html" $
        let expectedResp =
              "Hello World\n"
                { matchHeaders = ["Content-Type" <:> "text/html;charset=utf-8"]
                }
        in get "hello.html" `shouldRespondWith` expectedResp
      it "dir/inner-file.html responds correctly" $
        get "dir/inner-file.html" `shouldRespondWith` "Inner File\n"
      it "non existing file gives 404" $
        get "somefilethatdoesntexist.html" `shouldRespondWith` 404
