{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Servant.Static.TH.Internal.Server where

import Data.Foldable (foldl1)
import Data.List.NonEmpty (NonEmpty)
import Language.Haskell.TH
       (Dec, Exp, Q, appE, clause, conT, funD, mkName, normalB,
        runIO, sigD)
import Language.Haskell.TH.Syntax (addDependentFile)
import Servant.API ((:<|>)((:<|>)))
import Servant.Server (ServerT)

import Servant.Static.TH.Internal.FileTree
import Servant.Static.TH.Internal.Mime

combineWithExp :: Q Exp -> Q Exp -> Q Exp -> Q Exp
combineWithExp combiningExp = appE . appE combiningExp

combineWithServantOr :: NonEmpty (Q Exp) -> Q Exp
combineWithServantOr = foldl1 $ combineWithExp [e|(:<|>)|]

fileTreeToServer :: FileTree -> Q Exp
fileTreeToServer (FileTreeFile filePath fileContents) = do
  addDependentFile filePath
  MimeTypeInfo _ _ contentToExp <- extensionToMimeTypeInfoEx filePath
  contentToExp fileContents
fileTreeToServer (FileTreeDir _ fileTrees) =
  combineWithServantOr $ fmap fileTreeToServer fileTrees

-- | Take a template directory argument as a 'FilePath' and create a 'ServerT'
-- function that serves the files under the directory.  Empty directories will
-- be ignored.
--
-- Note that the file contents will be embedded in the function.  They will
-- not be served dynamically at runtime.  This makes it easy to create a
-- Haskell binary for a website with all static files completely baked-in.
--
-- For example, assume the following directory structure and file contents:
--
-- @
--   $ tree dir\/
--   dir\/
--   ├── js
--   │   └── test.js
--   └── index.html
-- @
--
-- @
--   $ cat dir\/index.html
--   \<p\>Hello World\<\/p\>
--   $ cat dir\/js\/test.js
--   console.log(\"hello world\");
-- @
--
-- 'createServerExp' is used like the following:
--
-- @
--   \{\-\# LANGUAGE DataKinds \#\-\}
--   \{\-\# LANGUAGE TemplateHaskell \#\-\}
--
--   type FrontEndAPI = $('Servant.Static.TH.Internal.API.createApiType' \"dir\")
--
--   frontEndServer :: 'Applicative' m => 'ServerT' FrontEndAPI m
--   frontEndServer = $('createServerExp' \"dir\")
-- @
--
-- At compile time, this expands to something like the following.  This has
-- been slightly simplified to make it easier to understand:
--
-- @
--   type FrontEndAPI =
--          \"js\" 'Servant.API.:>' \"test.js\" 'Servant.API.:>' 'Servant.API.Get' \'['JS'] 'Data.ByteString.ByteString'
--     ':<|>' \"index.html\" 'Servant.API.:>' 'Servant.API.Get' \'['Servant.HTML.Blaze.HTML'] 'Text.Blaze.Html.Html'
--
--   frontEndServer :: 'Applicative' m => 'ServerT' FrontEndAPI m
--   frontEndServer =
--          'pure' "console.log(\\"hello world\\");"
--     ':<|>' 'pure' "\<p\>Hello World\<\/p\>"
-- @
createServerExp
  :: FilePath
  -> Q Exp
createServerExp templateDir = do
  fileTree <- runIO $ getFileTreeIgnoreEmpty templateDir
  combineWithServantOr $ fmap fileTreeToServer fileTree

-- | This is similar to 'createServerExp', but it creates the whole function
-- declaration.
--
-- Given the following code:
--
-- @
--   \{\-\# LANGUAGE DataKinds \#\-\}
--   \{\-\# LANGUAGE TemplateHaskell \#\-\}
--
--   $('createServerDec' \"FrontAPI\" \"frontServer\" \"dir\")
-- @
--
-- You can think of it as expanding to the following:
--
-- @
--   frontServer :: 'Applicative' m => 'ServerT' FrontAPI m
--   frontServer = $('createServerExp' \"dir\")
-- @
createServerDec
  :: String   -- ^ name of the api type synonym
  -> String   -- ^ name of the server function
  -> FilePath -- ^ directory name to read files from
  -> Q [Dec]
createServerDec apiName serverName templateDir =
  let funcName = mkName serverName
      sigTypeQ =
          [t|forall m. Applicative m => ServerT $(conT (mkName apiName)) m|]
      signatureQ = sigD funcName sigTypeQ
      clauses = [clause [] (normalB (createServerExp templateDir)) []]
      funcQ = funD funcName clauses
  in sequence [signatureQ, funcQ]
