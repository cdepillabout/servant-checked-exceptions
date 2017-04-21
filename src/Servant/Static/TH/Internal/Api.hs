{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Static.TH.Internal.Api where

import Data.Foldable (foldl1)
import Data.List.NonEmpty (NonEmpty)
import Language.Haskell.TH
       (Dec, Q, Type, appT, litT, mkName,
        runIO, strTyLit, tySynD)
import Language.Haskell.TH.Syntax (addDependentFile)
import Servant.API (Get, (:<|>), (:>))
import System.FilePath (takeFileName)

import Servant.Static.TH.Internal.FileTree
import Servant.Static.TH.Internal.Mime

fileTreeToApiType :: FileTree -> Q Type
fileTreeToApiType (FileTreeFile filePath _) = do
  addDependentFile filePath
  MimeTypeInfo mimeT respT _ <- extensionToMimeTypeInfoEx filePath
  let fileNameLitT = litT $ strTyLit $ takeFileName filePath
  [t|$(fileNameLitT) :> Get '[$(mimeT)] $(respT)|]
fileTreeToApiType (FileTreeDir filePath fileTrees) =
  let fileNameLitT = litT $ strTyLit $ takeFileName filePath
  in [t|$(fileNameLitT) :> $(combineWithServantOrT nonEmptyApiTypesQ)|]
  where
    nonEmptyApiTypesQ :: NonEmpty (Q Type)
    nonEmptyApiTypesQ = fmap fileTreeToApiType fileTrees

-- | Given a list of @'Q' 'Type'@, combine them with Servant's '(:<|>)'
-- function and return the resulting @'Q' 'Type'@.
combineWithServantOrT :: NonEmpty (Q Type) -> Q Type
combineWithServantOrT = foldl1 $ combineWithType [t|(:<|>)|]

combineWithType :: Q Type -> Q Type -> Q Type -> Q Type
combineWithType combiningType = appT . appT combiningType

-- | Take a template directory argument as a 'FilePath' and create a Servant
-- type representing the files in the directory.  Empty directories will be
-- ignored.
--
-- For example, assume the following directory structure:
--
-- @
--   $ tree dir\/
--   dir\/
--   ├── js
--   │   └── test.js
--   └── index.html
-- @
--
-- 'createApiType' is used like the following:
--
-- @
--   \{\-\# LANGUAGE DataKinds \#\-\}
--   \{\-\# LANGUAGE TemplateHaskell \#\-\}
--
--   type FrontEndAPI = $('createApiType' \"dir\")
-- @
--
-- At compile time, it will expand to the following:
--
-- @
--   type FrontEndAPI =
--          \"js\" ':>' \"test.js\" ':>' 'Get' \'['JS'] 'Data.ByteString.ByteString'
--     ':<|>' \"index.html\" ':>' 'Get' \'['Servant.HTML.Blaze.HTML'] 'Text.Blaze.Html.Html'
-- @
createApiType
  :: FilePath -- ^ directory name to read files from
  -> Q Type
createApiType templateDir = do
  fileTree <- runIO $ getFileTreeIgnoreEmpty templateDir
  combineWithServantOrT $ fmap fileTreeToApiType fileTree

-- | This is similar to 'createApiType', but it creates the whole type synonym
-- declaration.
--
-- Given the following code:
--
-- @
--   \{\-\# LANGUAGE DataKinds \#\-\}
--   \{\-\# LANGUAGE TemplateHaskell \#\-\}
--
--   $('createApiDec' \"FrontAPI\" \"dir\")
-- @
--
-- You can think of it as expanding to the following:
--
-- @
--   type FrontAPI = $('createApiType' \"dir\")
-- @
createApiDec
  :: String   -- ^ name of the api type synonym
  -> FilePath -- ^ directory name to read files from
  -> Q [Dec]
createApiDec apiName templateDir =
  pure <$> tySynD (mkName apiName) [] (createApiType templateDir)
