# servant-static

This library provides a Template Haskell helper to generate Servant api, server and links for your static assets.
Under the hood, it uses [file-embed](https://hackage.haskell.org/package/file-embed) to serve your files directly from memory.
It also generates hashed names for maximum cachability.

## Usage

~~~ haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Exception (assert)
import Network.Wai.Handler.Warp (run)
import Servant.Static

mkApp "Static" "../static"

main :: IO ()
main = do
  assert (style_css == "style-cd2e5a9b48964e0cf2476057cc897b06.css") $ print "css link OK"
  assert (nested_script_js == "nested/script-548dd8cb607e8dbb634a60f52bbbcb86.js") $ print "script link OK"
  -- Use with Warp
  -- run 8080 appForStatic
~~~

## Inspirational works

- [servant-static-th](https://hackage.haskell.org/package/servant-static-th)
- [yesod-static](https://hackage.haskell.org/package/yesod-static)
