{-# LANGUAGE OverloadedStrings #-}

module Middleware (staticFiles) where

import Network.Wai
import Network.Wai.Middleware.Static
import System.FilePath

staticFiles :: Middleware
staticFiles = staticPolicy (only [("", "public/index.html")] <|> addBase "public")
