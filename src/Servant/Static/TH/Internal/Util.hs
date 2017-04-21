{- |
Module      :  Servant.Static.TH.Internal.Util

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

Utilities functions for use in this package.
-}

module Servant.Static.TH.Internal.Util where

import System.FilePath (takeExtension)

-- | Remove a leading period from a 'String'.
--
-- >>> removeLeadingPeriod ".jpg"
-- "jpg"
--
-- Just return the 'String' if it doesn't start with a period:
--
-- >>> removeLeadingPeriod "hello"
-- "hello"
--
-- Return an empty string if the only character in the string is a period:
--
-- >>> removeLeadingPeriod "."
-- ""
--
-- Remove at most one period:
--
-- >>> removeLeadingPeriod "..bye"
-- ".bye"
removeLeadingPeriod :: String -> String
removeLeadingPeriod ('.':chars) = chars
removeLeadingPeriod string = string

-- | Return an extension for a 'FilePath'.  Just like 'takeExtension', but
-- doesn't return the leading period.
--
-- >>> getExtension "/some/file.html"
-- "html"
--
-- Empty string is returned for files with no extension:
--
-- >>> getExtension "file"
-- ""
getExtension :: FilePath -> FilePath
getExtension = removeLeadingPeriod . takeExtension
