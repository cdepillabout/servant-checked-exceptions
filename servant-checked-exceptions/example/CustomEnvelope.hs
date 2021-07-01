{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (guard)
import Data.Aeson (ToJSON(..), (.=), object, FromJSON(..), (.:), withObject, decode)
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import qualified Data.Text as T
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status400)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import Text.Read (readMaybe, readEither)

import Servant.API (Capture, JSON, Post, (:>), (:<|>)((:<|>)), ToHttpApiData(..), FromHttpApiData(..))
import Servant.Client
       (BaseUrl(BaseUrl), Scheme(Http), ClientEnv(ClientEnv), ClientM, ClientError(FailureResponse),
        client, runClientM, responseBody)
import Servant.Docs
       (DocCapture(DocCapture), ToCapture(toCapture), ToSample(toSamples),
        docs, markdown)
import Servant.Server (Handler, ServerT, serve)

import Servant.Checked.Exceptions
       (Throws', NoThrow', FlatEnvelope(..), ErrStatus(..),
        catchesEnvelope, pureSuccEnvelope, emptyEnvelope, pureErrEnvelope)

-------------------------
-- Types and instances --
-------------------------

-- | Dummy Foo parameter type
newtype Foo = Foo Int deriving (Show, Read)

instance ToHttpApiData Foo where
  toUrlPiece (Foo n) = T.pack . show $ n

instance FromHttpApiData Foo where
  parseUrlPiece = fmap Foo . first T.pack . readEither . T.unpack

-- | Docs for Foo as uri parameter
instance ToCapture (Capture "foo" Foo) where
  toCapture Proxy =
    DocCapture "foo" "a search string containing an integer"


-- | Dummy Bar parameter type
data Bar = Bar deriving (Show, Read)

instance ToHttpApiData Bar where
  toUrlPiece = T.pack . show

instance FromHttpApiData Bar where
  parseUrlPiece = first T.pack . readEither . T.unpack

-- | Docs for Bar as uri parameter
instance ToCapture (Capture "bar" Bar) where
  toCapture Proxy =
    DocCapture "bar" "a search string containing \"Bar\""


-- | Dummy FooBar result type
-- When serialised, it is contained inside a "foobar" field.
data FooBar = FooBar String

instance ToJSON FooBar where
  toJSON (FooBar s) = object $ ["foobar" .= s]

instance FromJSON FooBar where
  parseJSON = withObject "FooBar" $ \o -> FooBar <$> o .: "foobar"

-- | Docs for FooBar as response
instance ToSample FooBar where
  toSamples Proxy = [("A successful FooBar response", FooBar "foo: 42")]

------------
-- Errors --
------------

-- | Dummy errors: these serialise in an "error" field.
data Err1 = Err1 deriving Show

instance ToJSON Err1 where
  toJSON Err1 = object $ [ "error" .= show Err1]

instance FromJSON Err1 where
  parseJSON = withObject "Err1" $ \o -> do
    e <- o .: "error"
    guard $ e == show Err1
    pure Err1

-- | Error status
instance ErrStatus Err1 where
  toErrStatus _ = status400

-- | Sample error
instance ToSample Err1 where
  toSamples Proxy = [("Error nr. 1", Err1)]


data Err2 = Err2 deriving Show

instance ToJSON Err2 where
  toJSON Err2 = object $ ["error" .= show Err2]

instance FromJSON Err2 where
  parseJSON = withObject "Err2" $ \o -> do
    e <- o .: "error"
    guard $ e == show Err2
    pure Err2

instance ErrStatus Err2 where
  toErrStatus _ = status400

instance ToSample Err2 where
  toSamples Proxy = [("Error nr. 2", Err2)]

---------
-- API --
---------

-- | This is our main 'Api' type. It defines two simple routes: one throws
-- errors, the second throws no errors. A custom envelope is used for both.
-- We will create a server, a client, and documentation for this api.
-- To understand what is different, compare with the modules 'Api', 'Client' and 'Server'.
type Api = "api" :> ( FooApi :<|> BarApi)

type FooApi = 
  Capture "foo" Foo :>
  Throws' FlatEnvelope Err1 :>
  Throws' FlatEnvelope Err2 :>
  Post '[JSON] FooBar

type BarApi = 
  Capture "bar" Bar :>
  NoThrow' FlatEnvelope :>
  Post '[JSON] FooBar

------------
-- Server --
------------

-- | This is our server root for the 'ServerT' for 'Api'.  We have two handlers.
serverRoot :: ServerT Api Handler
serverRoot = fooHandler :<|> barHandler

-- | This is the handler for 'FooApi'.
-- If the integer contained in 'Foo' is '1' or '2', we respectively throw an
-- 'Err1' or 'Err2'. Otherwise, we return success.
-- We use standard envelopes, and at the outer layer wrap it with a custom
-- envelope. This allows to reuse existing envelope functions and 'EnvelopeT',
-- but still have custom JSON instances. You can of course define your own
-- custom envelope type which does not use 'Envelope' internally.
fooHandler :: Foo -> Handler (FlatEnvelope '[Err1, Err2] FooBar)
fooHandler (Foo 1) = fmap FlatEnvelope . pureErrEnvelope $ Err1
fooHandler (Foo 2) = fmap FlatEnvelope . pureErrEnvelope $ Err2
fooHandler (Foo n) = fmap FlatEnvelope . pureSuccEnvelope . FooBar $ "foo: " <> show n

-- | This is the handler for 'BarApi'.
-- The handler just returns success, throwing no errors.
barHandler :: Bar -> Handler (FlatEnvelope '[] FooBar)
barHandler Bar = fmap FlatEnvelope . pureSuccEnvelope . FooBar $ "bar"

-- | Create a WAI 'Application'.
app :: Application
app = serve (Proxy :: Proxy Api) serverRoot

-- | Run the WAI 'Application' using 'run' on the port defined by 'port'.
serverMain :: IO ()
serverMain = run port app

------------
-- Client --
------------

-- | We generate the client functions just like normal.  Note that when we use
-- 'Throws' or 'NoThrow', the client functions get generated with the
-- 'FlatEnvelope' type.
fooClient
  :: Foo
  -> ClientM (FlatEnvelope '[Err1, Err2] FooBar)
barClient
  :: Bar
  -> ClientM (FlatEnvelope '[] FooBar)
(fooClient :<|> barClient) = client (Proxy :: Proxy Api)

-- | This function uses the 'fooClient' function to send a 'Foo' to
-- the server.
--
-- Note how 'catchesEnvelope' is used to handle the two error reponses and the
-- success response.
-- To mitigate servant-checked-exceptions#27, we try to reparse the error
-- response on a 'FailureResponse', and get our envelope anyway.
runFoo :: ClientEnv -> Foo -> IO ()
runFoo clientEnv foo = do
  eitherRes <- runClientM (fooClient foo) clientEnv
  case eitherRes of
    Left servantErr@(FailureResponse _ resp) -> case decode . responseBody $ resp of
      Just fenv -> handleEnvelope fenv
      Nothing -> handleServantError servantErr
    Left servantErr -> handleServantError servantErr
    Right fenv -> handleEnvelope fenv
  where 
    handleEnvelope :: FlatEnvelope '[Err1, Err2] FooBar -> IO ()
    handleEnvelope (FlatEnvelope env) =
      putStrLn $
        catchesEnvelope
          ( \Err1 -> "Failure: Err1"
          , \Err2 -> "Failure: Err2"
          )
          (\(FooBar foobar) -> "Success: " <> show foobar)
          env
    handleServantError servantErr = putStrLn $ "Got a ServantErr: " <> show servantErr

-- | This function uses the 'barClient' function to send a 'Bar' to
-- the server.
--
-- Note that we can just use 'emptyEnvelope' to get the result, as we know there
-- are no errors.
-- Also the above fix is not necessary, as we don't have errors.
runBar :: ClientEnv -> Bar -> IO ()
runBar clientEnv bar = do
  eitherRes <- runClientM (barClient bar) clientEnv
  case eitherRes of
    Left servantErr -> putStrLn $ "Got a ServantErr: " <> show servantErr
    Right (FlatEnvelope env) -> do
      let (FooBar foobar) = emptyEnvelope env
      putStrLn $ "Success: " <> show foobar

-- | Run 'runFoo' or 'runBar' depending on the given CLI argument
runClient :: ClientEnv -> String -> IO ()
runClient clientEnv query | query == "Bar" || query == "bar"
  = runBar clientEnv Bar
runClient clientEnv s
  = case readMaybe s of
    Just n -> runFoo clientEnv $ Foo n
    Nothing -> putStrLn $ "Not a valid query: " <> s

-- | Run the client
clientMain :: IO ()
clientMain = do
  manager <- newManager defaultManagerSettings
  args <- getArgs
  query <- case args of
    q : _ -> pure q
    _ -> putStrLn "No args given, using 'bar'"$> "bar"
  let clientEnv = ClientEnv manager baseUrl Nothing
  runClient clientEnv query

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" port ""

----------
-- Main --
----------

-- | Run everything:
-- 1. generate and print documentation
-- 2. start the server forked
-- 3. run the client
main :: IO ()
main = do
  putStrLn . markdown . docs $ Proxy @Api
  _ <- forkIO serverMain
  clientMain

-- | The port to run the server on.
port :: Int
port = 8201
