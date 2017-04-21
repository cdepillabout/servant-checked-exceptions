
{- |
Module      :  Servant.Static.TH

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module provides the 'createApiAndServerDecs' function.  At compile time,
it will read all the files under a specified directory, embed their contents,
create a Servant \"API\" type synonym representing their directory layout, and
create a 'ServerT' function for serving their contents statically.

Let's assume that we have a directory called @\"dir\"@ in the root of our
Haskell web API that looks like this:

@
  $ tree dir\/
  dir\/
  ├── js
  │   └── test.js
  └── hello.html
@

Here's the contents of @\"hello.html\"@ and @\"js\/test.js\"@:

@
  $ cat dir\/index.html
  \<p\>Hello World\<\/p\>
  $ cat dir\/js\/test.js
  console.log(\"hello world\");
@

The 'createApiAndServerDecs' function can be used like the following:

@
  \{\-\# LANGUAGE DataKinds \#\-\}
  \{\-\# LANGUAGE TemplateHaskell \#\-\}

  import "Data.Proxy" ('Data.Proxy.Proxy'('Data.Proxy.Proxy'))
  import "Network.Wai" ('Network.Wai.Application')
  import "Network.Wai.Handler.Warp" ('Network.Wai.Handler.Warp.run')
  import "Servant.Server" ('Servant.Server.serve')
  import "Servant.Static.TH" ('createApiAndServerDecs')

  $(createApiAndServerDecs \"FrontEndApi\" \"frontEndServer\" \"dir\")

  app :: 'Network.Wai.Application'
  app = 'Servant.Server.serve' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' FrontEndApi) frontEndServer

  main :: IO ()
  main = 'Network.Wai.Handler.Warp.run' 8080 app
@

'createApiAndServerDecs' will expand to something like the following at
compile time:

@
  type FrontEndAPI =
         \"js\" 'Servant.API.:>' \"test.js\" 'Servant.API.:>' 'Servant.API.Get' \'['JS'] 'Data.ByteString.ByteString'
    ':<|>' \"index.html\" 'Servant.API.:>' 'Servant.API.Get' \'['HTML'] 'Html'

  frontEndServer :: 'Applicative' m => 'Servant.Server.ServerT' FrontEndAPI m
  frontEndServer =
         'pure' "console.log(\\"hello world\\");"
    ':<|>' 'pure' "\<p\>Hello World\<\/p\>"
@

If this WAI application is running, it is possible to use @curl@ to access
the server:

@
  $ curl localhost:8080\/hello.html
  \<p\>Hello World\<\/p\>
  $ curl localhost:8080\/js\/test.js
  console.log(\"hello world\");
@

This 'createApiAndServerDecs' function is convenient to use when you want to
make a Servant application easy to deploy.  All the static frontend files are
bundled into the Haskell binary at compile-time, so all you need to do is
deploy the Haskell binary.  This works well for low-traffic websites like
prototypes and internal applications.

This shouldn't be used for high-traffic websites.  Instead, you should serve
your static files from something like Apache, nginx, or a CDN.
-}

module Servant.Static.TH
  ( -- * Create API
    createApiType
  , createApiDec
    -- * Create Server
  , createServerExp
  , createServerDec
    -- * Create Both API and Server
  , createApiAndServerDecs
    -- * MIME Types

    -- | The following types are the MIME types supported by servant-static-th.
    -- If you need additional MIME types supported, feel free to create an
    -- <https://github.com/cdepillabout/servant-static-th/issues issue> or
    -- <https://github.com/cdepillabout/servant-static-th/pulls PR>.
  , CSS
  , GIF
  , HTML
  , Html
  , JPEG
  , JS
  , PNG
  , SVG
  , TXT
    -- * Easy-To-Use Names and Paths

    -- | The functions in this section pick defaults for the template
    -- directory, api name, and the server function name. This makes it easy to
    -- use for quick-and-dirty code.

    -- ** Paths and Names
  , frontEndTemplateDir
  , frontEndApiName
  , frontEndServerName
    -- ** API
  , createApiFrontEndType
  , createApiFrontEndDec
    -- ** Server
  , createServerFrontEndExp
  , createServerFrontEndDec
    -- ** Server and API
  , createApiAndServerFrontEndDecs
  ) where

import Language.Haskell.TH (Dec, Exp, Q, Type, mkName, tySynD)
import System.FilePath ((</>))
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html)

import Servant.Static.TH.Internal
       (CSS, GIF, JPEG, JS, PNG, SVG, TXT, createApiDec, createApiType,
        createServerDec, createServerExp)

------------------------------------
-- Hard-coded Frontend file paths --
------------------------------------

-- | This is the directory @\"frontend\/dist\"@.
frontEndTemplateDir :: FilePath
frontEndTemplateDir = "frontend" </> "dist"

-- | This is the 'String' @\"FrontEnd\"@.
frontEndApiName :: String
frontEndApiName = "FrontEnd"

-- | This is the 'String' @\"frontEndServer\"@.
frontEndServerName :: String
frontEndServerName = "frontEndServer"

---------
-- Api --
---------

-- | This is the same as @'createApiType' 'frontEndTemplateDir'@.
createApiFrontEndType :: Q Type
createApiFrontEndType = createApiType frontEndTemplateDir

-- | This is the same as
-- @'createApiDec' 'frontEndApiName' 'frontEndTemplateDir'@.
createApiFrontEndDec :: Q [Dec]
createApiFrontEndDec =
  pure <$> tySynD (mkName "FrontEnd") [] createApiFrontEndType

------------
-- Server --
------------

-- | This is the same as @'createServerExp' 'frontEndTemplateDir'@.
createServerFrontEndExp :: Q Exp
createServerFrontEndExp = createServerExp frontEndTemplateDir

-- | This is the same as
-- @'createServerDec' 'frontEndApiName' 'frontEndServerName' 'frontEndTemplateDir'@.
createServerFrontEndDec :: Q [Dec]
createServerFrontEndDec =
  createServerDec frontEndApiName frontEndServerName frontEndTemplateDir

--------------------
-- Server and API --
--------------------

-- | This is a combination of 'createApiDec' and 'createServerDec'.  This
-- function is the one most users should use.
--
-- Given the following code:
--
-- @
--   \{\-\# LANGUAGE DataKinds \#\-\}
--   \{\-\# LANGUAGE TemplateHaskell \#\-\}
--
--   $('createApiAndServerDecs' \"FrontAPI\" \"frontServer\" \"dir\")
-- @
--
-- You can think of it as expanding to the following:
--
-- @
--   $('createApiDec' \"FrontAPI\" \"dir\")
--
--   $('createServerDec' \"FrontAPI\" \"frontServer\" \"dir\")
-- @
createApiAndServerDecs
  :: String   -- ^ name of the api type synonym
  -> String   -- ^ name of the server function
  -> FilePath -- ^ directory name to read files from
  -> Q [Dec]
createApiAndServerDecs apiName serverName templateDir =
  let apiDecs = createApiDec apiName templateDir
      serverDecs = createServerDec apiName serverName templateDir
  in mappend <$> apiDecs <*> serverDecs

-- | This is the same as
-- @'createApiAndServerDecs' 'frontEndApiName' 'frontEndServerName' 'frontEndTemplateDir'@.
createApiAndServerFrontEndDecs :: Q [Dec]
createApiAndServerFrontEndDecs =
  createApiAndServerDecs frontEndApiName frontEndServerName frontEndTemplateDir
