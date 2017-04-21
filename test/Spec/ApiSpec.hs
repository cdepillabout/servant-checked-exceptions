{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Spec.ApiSpec where

import Data.ByteString (ByteString)
import Data.Type.Equality ((:~:)(Refl))
import Servant.HTML.Blaze (HTML)
import Servant.API ((:<|>), (:>), Get)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Text.Blaze.Html (Html)

import Servant.Static.TH.Internal (JS, createApiDec)

import Spec.TestDirLocation (testDir)

$(createApiDec "FrontEndApi" testDir)

type ExpectedFrontEndApi =
    (
      "dir" :>
        (
          ( "inner-file.html" :> Get '[HTML] Html )
        :<|>
          ( "test.js" :> Get '[JS] ByteString )
        )
    )
  :<|>
    ( "hello.html" :> Get '[HTML] Html )

checkFrontEndApiType :: ExpectedFrontEndApi :~: FrontEndApi
checkFrontEndApiType = Refl

createdCorrectlyTest :: TestTree
createdCorrectlyTest =
  testCase "created correctly" $ checkFrontEndApiType @?= Refl

apiTests :: TestTree
apiTests = testGroup "api" [createdCorrectlyTest]
