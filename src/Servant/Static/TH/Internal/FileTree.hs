{- |
Module      :  Servant.Static.TH.Internal.FileTree

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

Read a directory and the contents of all the files in it as a 'FileTree'.
-}

module Servant.Static.TH.Internal.FileTree where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import System.Directory
       (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

-- | This tree structure represents the directory structure on disk.
data FileTree
  = FileTreeFile FilePath ByteString
  -- ^ A file with it's 'FilePath' and contents as a 'ByteString'.
  | FileTreeDir FilePath (NonEmpty FileTree)
  -- ^ A directory with it's 'FilePath' and the files under it.
  deriving (Eq, Read, Show)

-- | This is a simple version of 'FileTree', just used for tagging a given
-- 'FilePath' as a directory or a file.
data FileType
  = FileTypeFile FilePath
  | FileTypeDir FilePath
  deriving (Eq, Read, Show)

-- | Get the 'FileType' for a given 'FilePath'.  Calls 'fail' if it is not a
-- file or a directory.
getFileType :: FilePath -> IO FileType
getFileType path = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  case (isFile, isDir) of
    (True, _) -> pure $ FileTypeFile path
    (_, True) -> pure $ FileTypeDir path
    _ ->
      fail $
        "getFileType: Could not determine the type of file \"" <> path <> "\""

-- | Convert a 'FileType' to a 'FileTree'.  Return 'Nothing' if the input
-- 'FileType' is 'FileTypeDir', and that directory is empty.
fileTypeToFileTree :: FileType -> IO (Maybe FileTree)
fileTypeToFileTree (FileTypeFile filePath) =
  Just . FileTreeFile filePath <$> ByteString.readFile filePath
fileTypeToFileTree (FileTypeDir dir) = do
  fileTrees <- getFileTree dir
  pure $
    case fileTrees of
      [] -> Nothing
      (ft:fts) -> Just . FileTreeDir dir $ ft :| fts

-- | Convert an input directory 'FilePath' to a 'FileTree'.  Fails if the input
-- directory 'FilePath' is not a directory.
getFileTree :: FilePath -> IO [FileTree]
getFileTree templateDir = do
  filePaths <- sort <$> listDirectory templateDir
  let fullFilePaths = fmap (templateDir </>) filePaths
  fileTypes <- traverse getFileType fullFilePaths
  fileTreesWithMaybe <- traverse fileTypeToFileTree fileTypes
  pure $ catMaybes fileTreesWithMaybe

-- | Just like 'getFileTree', but returns an error with 'fail' if the input
-- directory is empty.
getFileTreeIgnoreEmpty :: FilePath -> IO (NonEmpty FileTree)
getFileTreeIgnoreEmpty templateDir = do
  fileTrees <- getFileTree templateDir
  case fileTrees of
    [] ->
      fail $
        "getFileTreeIgnoreEmpty: Top level template directory is empty: \"" <>
        templateDir <> "\""
    (ft:fts) -> pure $ ft :| fts
