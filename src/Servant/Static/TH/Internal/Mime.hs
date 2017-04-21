{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      :  Servant.Static.TH.Internal.Mime

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module exports functions and datatypes for using many different mime
types.
-}

module Servant.Static.TH.Internal.Mime where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy (Proxy)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Typeable (Typeable)
import Language.Haskell.TH (Exp, Q, Type)
import Network.HTTP.Media (MediaType, (//))
import Servant.HTML.Blaze (HTML)
import Servant.API (Accept(contentType), MimeRender(mimeRender))
import System.FilePath (takeExtension)
import Text.Blaze.Html (Html, preEscapedToHtml)

import Servant.Static.TH.Internal.Util
       (getExtension, removeLeadingPeriod)

-- | Hold 'Type's and functions work working with a given file type, like
-- @html@ or @js@.
--
-- You can find examples of 'MimeTypeInfo' in the function
-- 'extensionMimeTypeMap'.
data MimeTypeInfo = MimeTypeInfo
  { mimeTypeInfoContentType :: Q Type
    -- ^ A @'Q' 'Type'@ representing a type to use for the content type of a
    -- Servant API.  For instance, HTML files will use something like
    -- @[t|'HTML'|]@, while Javascript files will use something like
    -- @[t|'JS'|]@.
  , mimeTypeInfoRespType :: Q Type
    -- ^ A @'Q' 'Type'@ representing the type to use for the return vale of a
    -- Servant API.  For instance, HTML files will use something like
    -- @[t|'Html'|]@, while JavascriptFiles will use something like
    -- @[t|'ByteString'|]@.
  , mimeTypeInfoToExpression :: ByteString -> Q Exp
    -- ^ A function that turns a 'ByteString' into an 'Exp'.  For an example,
    -- look at 'htmlToExp' and 'byteStringtoExp'.
  }

byteStringToExp :: ByteString -> Q Exp
byteStringToExp byteString =
  let word8List = ByteString.unpack byteString
  in [e|pure $ ByteString.pack word8List|]

htmlToExp :: ByteString -> Q Exp
htmlToExp byteString =
  let fileContentsString = unpack $ decodeUtf8With lenientDecode byteString
  in [e|pure $ (preEscapedToHtml :: String -> Html) fileContentsString|]

-- | A mapping from an extension like @html@ or @js@ to a 'MimeTypeInfo' for
-- that extension.
extensionMimeTypeMap :: Map String MimeTypeInfo
extensionMimeTypeMap =
  [ ("css", MimeTypeInfo [t|CSS|] [t|ByteString|] byteStringToExp)
  , ("gif", MimeTypeInfo [t|GIF|] [t|ByteString|] byteStringToExp)
  , ("htm", MimeTypeInfo [t|HTML|] [t|Html|] htmlToExp)
  , ("html", MimeTypeInfo [t|HTML|] [t|Html|] htmlToExp)
  , ("jpeg", MimeTypeInfo [t|JPEG|] [t|ByteString|] byteStringToExp)
  , ("jpg", MimeTypeInfo [t|JPEG|] [t|ByteString|] byteStringToExp)
  , ("js", MimeTypeInfo [t|JS|] [t|ByteString|] byteStringToExp)
  , ("png", MimeTypeInfo [t|PNG|] [t|ByteString|] byteStringToExp)
  , ("txt", MimeTypeInfo [t|TXT|] [t|ByteString|] byteStringToExp)
  ]

-- | Just like 'extensionToMimeTypeInfo', but throw an error using 'fail' if
-- the extension for the given 'FilePath' is not found.
extensionToMimeTypeInfoEx :: FilePath -> Q MimeTypeInfo
extensionToMimeTypeInfoEx file =
  case extensionToMimeTypeInfo file of
    Just mimeTypeInfo -> pure mimeTypeInfo
    Nothing ->
      let extension = getExtension file
      in fail $
        "Unknown extension type \"" <> extension <> "\".  Please report as bug."

-- | Lookup the 'MimeTypeInfo' for a given 'FilePath' (that has an extension
-- like @.html@ or @.js@).  Returns 'Nothing' if the 'MimeTypeInfo' for the
-- given extension is not found.
extensionToMimeTypeInfo :: FilePath -> Maybe MimeTypeInfo
extensionToMimeTypeInfo file =
  Map.lookup
    (removeLeadingPeriod $ takeExtension file)
    extensionMimeTypeMap

-------------------------
-- Supported MimeTypes --
-------------------------

-- CSS

data CSS deriving Typeable

-- | @text\/css@
instance Accept CSS where
  contentType :: Proxy CSS -> MediaType
  contentType _ = "text" // "css"

instance MimeRender CSS ByteString where
  mimeRender :: Proxy CSS -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

-- GIF

data GIF deriving Typeable

-- | @image\/gif@
instance Accept GIF where
  contentType :: Proxy GIF -> MediaType
  contentType _ = "image" // "gif"

instance MimeRender GIF ByteString where
  mimeRender :: Proxy GIF -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

-- JPEG

data JPEG deriving Typeable

-- | @image\/jpeg@
instance Accept JPEG where
  contentType :: Proxy JPEG -> MediaType
  contentType _ = "image" // "jpeg"

instance MimeRender JPEG ByteString where
  mimeRender :: Proxy JPEG -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

-- JS

data JS deriving Typeable

-- | @application\/javascript@
instance Accept JS where
  contentType :: Proxy JS -> MediaType
  contentType _ = "application" // "javascript"

instance MimeRender JS ByteString where
  mimeRender :: Proxy JS -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

-- PNG

data PNG deriving Typeable

-- | @image\/png@
instance Accept PNG where
  contentType :: Proxy PNG -> MediaType
  contentType _ = "image" // "png"

instance MimeRender PNG ByteString where
  mimeRender :: Proxy PNG -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

-- SVG

data SVG deriving Typeable

-- | @image\/svg@
instance Accept SVG where
  contentType :: Proxy SVG -> MediaType
  contentType _ = "image" // "svg"

instance MimeRender SVG ByteString where
  mimeRender :: Proxy SVG -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

-- TXT

data TXT deriving Typeable

-- | @text\/plain@
instance Accept TXT where
  contentType :: Proxy TXT -> MediaType
  contentType _ = "text" // "plain"

instance MimeRender TXT ByteString where
  mimeRender :: Proxy TXT -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict
